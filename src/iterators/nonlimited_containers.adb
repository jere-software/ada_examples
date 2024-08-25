-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with System.Address_To_Access_Conversions;

package body Nonlimited_Containers is

   -- Use Address_To_Access_Conversions to bypass const-ness of container.
   -- Below are helper functions for that
   package A2A is new System.Address_To_Access_Conversions(Container);
   function Mutate(Self : Container) return not null access Container is
      (A2A.To_Pointer(Self'Address)) with Inline;
   function Mutate
      (Self : not null access constant Container) 
       return not null access Container
   is (Mutate(Self.all)) with Inline;

   function Keep_Iterating(Position : Cursor) return Boolean is (Position.Value /= 0);

   function Reference
      (Self     : aliased in out Container;
       Position : Cursor)
       return Element_Reference
   is (Source => Position.Source, Element => Self.Elements(Position.Value)'Access);

   function Constant_Reference
      (Self     : aliased Container;
       Position : Cursor)
       return Constant_Element_Reference
   is (Source => Position.Source, Element => Self.Elements(Position.Value)'Access);

   procedure Finalize(Self : in out Container) is 
   begin
      if Self.Tamper_Count /= 0 then
         raise Program_Error with "Attempting to destroy container with iterators still active";
      end if;
   end Finalize;

   -- Main iterator type.  Internal to package so outside world cannot 
   -- use this directly
   type Iterator
      (Source : not null access constant Container) 
   is new Ada.Finalization.Limited_Controlled
      and Iterator_Kind
   with record
      Finalized : Boolean := False; -- To make sure Finalization is idempotent
   end record;
   
   -- Parent type and interface overrides
   overriding procedure Finalize(Self : in out Iterator);
   overriding function First(Self : Iterator) return Cursor;
   overriding function Next(Self : Iterator; Position : Cursor) return Cursor;

   procedure Finalize(Self : in out Iterator) is
      Proxy : not null access Container := Mutate(Self.Source);
   begin
      if not Self.Finalized then 
         Self.Finalized := True;

         -- Since Self.Source points to a constant container
         -- use the Rosen technique variable to use it 
         -- mutably
         Proxy.Tamper_Count := @ - 1;
      end if;
   end Finalize;

   function First(Self : Iterator) return Cursor is
      (Source => Self'Access, Value => 1);

   function Next(Self : Iterator; Position : Cursor) return Cursor is
      (Source => Self'Access, 
       Value  => (if Position.Value = 0 then 
                     0 
                  elsif Position.Value = Self.Source.Count then
                     0
                  else Position.Value + 1));

   function Iterate(Self : aliased Container) return Iterator_Kind'Class is 
      Proxy : not null access Container := Mutate(Self);
   begin
      return Result : Iterator(Self'Access) do 
         -- Since Self is passed in as constant ("in" mode), need to use the
         -- Rosen technique to update the tamper count to ensure the
         -- container cannot be modified while the iterator exists
         Proxy.Tamper_Count := @ + 1;
      end return;
   end Iterate;

end Nonlimited_Containers;