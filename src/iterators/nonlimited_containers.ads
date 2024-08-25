-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

private with Ada.Finalization;
with Ada.Iterator_Interfaces;

-- Example type for iterating over a limited container.
generic

   -- Main type for the element held in the container
   type Element_Type is private;

package Nonlimited_Containers is

   -- Main container type
   type Container(Count : Natural) is tagged private
      with Constant_Indexing => Constant_Reference,
           Variable_Indexing => Reference,
           Default_Iterator  => Iterate,
           Iterator_Element  => Element_Type;

   ----------------------------------------------------
   -- Stable iteration
   -- NOTE: The following is only for "for of" and
   --       "for in" loops.  Make a different cursor
   --       and reference types for any cursors you 
   --       want to be saved off outside the container
   ----------------------------------------------------

   -- Main iteration cursor type
   type Cursor(<>) is private;

   -- Operation for Ada.Iterator_Interfaces
   function Keep_Iterating(Position : Cursor) return Boolean;

   -- Ada.Iterator_Interfaces implementation
   package Iterators is new Ada.Iterator_Interfaces(Cursor, Keep_Iterating);

   -- Choose the parent type for the iterator
   subtype Iterator_Kind is Iterators.Forward_Iterator;

   -- Default iteration operation.  Paramter must be
   -- constant to allow for constant iteration loops, but
   -- the implementation must treat it as variable for 
   -- variable loop iteration
   function Iterate(Self : aliased Container) return Iterator_Kind'Class;

   -- Main reference type
   type Element_Reference
      (Element : not null access Element_Type;
       Source  : not null access constant Iterator_Kind'Class)  -- Used to help Ada catch dangling pointer mistakes
   is limited private
      with Implicit_Dereference => Element;

   -- Returns a reference based on the Cursor
   function Reference
      (Self     : aliased in out Container;
       Position : Cursor)
       return Element_Reference;

   -- Main constant reference type
   type Constant_Element_Reference
      (Element : not null access constant Element_Type;
       Source  : not null access constant Iterator_Kind'Class)  -- Used to help Ada catch dangling pointer mistakes
   is limited private
      with Implicit_Dereference => Element;

   -- Returns a constant reference based on the Cursor
   function Constant_Reference
      (Self     : aliased Container;
       Position : Cursor)
       return Constant_Element_Reference;

private

      -- A collection of elements.  Aliased so I can take 'Access later
   type Element_Array is array(Positive range <>) of aliased Element_Type;

   type Container
      (Count : Natural) 
   is 
      new Ada.Finalization.Controlled
   with record

      -- List of dummy elements to iterate over
      Elements : Element_Array(1..Count);

      -- This is used to ensure that the container cannot
      -- be modified or destroyed while an iterator exists
      Tamper_Count : Natural := 0;

   end record;

   -- Parent overrides
   overriding procedure Finalize(Self : in out Container);

   -- The cursor has a back reference to the iterator so 
   -- hopefully the Ada compiler can catch dangling reference
   -- mistakes
   type Cursor
      (Source : not null access constant Iterator_Kind'Class) 
   is record
      Value : Natural := 0;
   end record;

   type Element_Reference
      (Element : not null access Element_Type;
       Source  : not null access constant Iterator_Kind'Class)  -- Used to help Ada catch dangling pointer mistakes
   is limited null record;

   type Constant_Element_Reference
      (Element : not null access constant Element_Type;
       Source  : not null access constant Iterator_Kind'Class)  -- Used to help Ada catch dangling pointer mistakes
   is limited null record;

end Nonlimited_Containers;