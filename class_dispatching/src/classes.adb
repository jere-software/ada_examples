with Ada.Text_IO; use Ada.Text_IO;

package body Classes is

   ----------------------------------------------
   -- Great_Grand_Parent
   ----------------------------------------------

   procedure Print(Self : Great_Grand_Parent) is
   begin
      Put_Line("Great_Grand_Parent.Print");
   end Print;

   procedure Print_With_Redispatch(Self : Great_Grand_Parent) is
   begin
      Great_Grand_Parent'Class(Self).Print;
   end Print_With_Redispatch;

   procedure Print_Without_Redispatch(Self : Great_Grand_Parent) is 
   begin
      Self.Print;
   end Print_Without_Redispatch;

   procedure Print_As_Great_Grand_Parent(Self : Great_Grand_Parent'Class) is
   begin
      Great_Grand_Parent(Self).Print;
   end Print_As_Great_Grand_Parent;

   procedure Print_As_Latest_Descendent(Self : Great_Grand_Parent'Class) is
   begin
      Self.Print;
   end Print_As_Latest_Descendent;


   ----------------------------------------------
   -- Great_Parent
   ----------------------------------------------

   procedure Print(Self : Grand_Parent) is
   begin
      Put_Line("Grand_Parent.Print");
   end Print;

   ----------------------------------------------
   -- Parent
   ----------------------------------------------

   procedure Print(Self : Parent) is
   begin
      Put_Line("Parent.Print");
   end Print;

   ----------------------------------------------
   -- Parent
   ----------------------------------------------

   procedure Print(Self : Child) is
   begin
      Put_Line("Child.Print");
   end Print;

   ----------------------------------------------
   -- Aunt
   ----------------------------------------------

   procedure Print(Self : Aunt) is
   begin
      Put("Aunt.Print: ");
      Grand_Parent(Self).Print;
   end Print;

   ----------------------------------------------
   -- Cousin
   ----------------------------------------------

   procedure Print(Self : Cousin) is
   begin
      Put_Line("Cousin.Print");
   end Print;

end Classes;