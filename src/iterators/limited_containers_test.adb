with Limited_Containers;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

procedure Limited_Containers_Test is

   package Integers is new Limited_Containers(Integer);

   Test : Integers.Container(5);

   Value : Integer := 1;

   type Container_Access is access Integers.Container;

   Test_Bad : Container_Access := new Integers.Container(5);

   procedure Free is new Ada.Unchecked_Deallocation(Integers.Container, Container_Access);

begin
   New_Line;
   Put_Line("Initial Values:");

   -- Print junk inital values
   for Element of Test loop
      Put_Line(Element'Image);
   end loop;

   -- Set them all to 0
   for Element of Test loop
      Element := 0;
   end loop;

   New_Line;
   Put_Line("All Zeroes");

   -- Print 0 values
   for Element of Test loop
      Put_Line(Element'Image);
   end loop;

   -- Set them all to 0
   for Element of Test loop
      Element := Value;
      Value   := Value + 1;
   end loop;

   New_Line;
   Put_Line("Incremental Values");

   -- Print 0 values
   for Element of Test loop
      Put_Line(Element'Image);
   end loop;

   -- Test tamper count
   begin
      for Element of Test_Bad.all loop
         Free(Test_Bad);
      end loop;
      New_Line;
      Put_Line("Failed to catch tamper violation");
   exception
      when others => 
         New_Line;
         Put_Line("exception!!");
   end;

end Limited_Containers_Test;