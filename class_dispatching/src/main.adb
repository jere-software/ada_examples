-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Text_IO; use Ada.Text_IO;
with Classes;

-- Program entry point
procedure Main is

   Great_Grand_Parent : Classes.Great_Grand_Parent;
   Grand_Parent       : Classes.Grand_Parent;
   Parent             : Classes.Parent;
   Child              : Classes.Child;
   Uncle              : Classes.Uncle;
   Aunt               : Classes.Aunt;
   Cousin             : Classes.Cousin;

begin
   New_Line;
   Put_Line("Call everyone's print");
   Put_Line("---------------------");
   Great_Grand_Parent.Print;
   Grand_Parent.Print;
   Parent.Print;
   Child.Print;
   Uncle.Print; -- Doesn't override print
   Aunt.Print;  -- Calls her parent Print internally
   Cousin.Print;

   New_Line;
   Put_Line("Call Print for everyone as Great_Grand_Parent");
   Put_Line("-------------------------------------------");
   Classes.Great_Grand_Parent(Great_Grand_Parent).Print;
   Classes.Great_Grand_Parent(Grand_Parent).Print;
   Classes.Great_Grand_Parent(Parent).Print;
   Classes.Great_Grand_Parent(Child).Print;
   Classes.Great_Grand_Parent(Uncle).Print;
   Classes.Great_Grand_Parent(Aunt).Print;
   Classes.Great_Grand_Parent(Cousin).Print;

   New_Line;
   Put_Line("Call Print_With_Redispatch for everyone as Great_Grand_Parent");
   Put_Line("---------------------");
   Classes.Great_Grand_Parent(Great_Grand_Parent).Print_With_Redispatch;
   Classes.Great_Grand_Parent(Grand_Parent).Print_With_Redispatch;
   Classes.Great_Grand_Parent(Parent).Print_With_Redispatch;
   Classes.Great_Grand_Parent(Child).Print_With_Redispatch;
   Classes.Great_Grand_Parent(Uncle).Print_With_Redispatch;
   Classes.Great_Grand_Parent(Aunt).Print_With_Redispatch;
   Classes.Great_Grand_Parent(Cousin).Print_With_Redispatch;


   New_Line;
   Put_Line("Call Print_Without_Redispatch for everyone");
   Put_Line("---------------------");
   Great_Grand_Parent.Print_Without_Redispatch;
   Grand_Parent.Print_Without_Redispatch;
   Parent.Print_Without_Redispatch;
   Child.Print_Without_Redispatch;
   Uncle.Print_Without_Redispatch;
   Aunt.Print_Without_Redispatch;
   Cousin.Print_Without_Redispatch;

   New_Line;
   Put_Line("Call Print_As_Great_Grand_Parent for everyone");
   Put_Line("---------------------");
   Classes.Print_As_Great_Grand_Parent(Great_Grand_Parent);
   Classes.Print_As_Great_Grand_Parent(Grand_Parent);
   Classes.Print_As_Great_Grand_Parent(Parent);
   Classes.Print_As_Great_Grand_Parent(Child);
   Classes.Print_As_Great_Grand_Parent(Uncle);
   Classes.Print_As_Great_Grand_Parent(Aunt);
   Classes.Print_As_Great_Grand_Parent(Cousin);

   New_Line;
   Put_Line("Call Print_As_Latest_Descendent for everyone");
   Put_Line("---------------------");
   Classes.Print_As_Latest_Descendent(Great_Grand_Parent);
   Classes.Print_As_Latest_Descendent(Grand_Parent);
   Classes.Print_As_Latest_Descendent(Parent);
   Classes.Print_As_Latest_Descendent(Child);
   Classes.Print_As_Latest_Descendent(Uncle);
   Classes.Print_As_Latest_Descendent(Aunt);
   Classes.Print_As_Latest_Descendent(Cousin);

end Main;