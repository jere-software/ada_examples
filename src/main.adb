-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Text_IO;

with Limited_Containers_Test;

-- Program entry point
procedure Main is
begin
   Ada.Text_IO.Put_Line("Hello World");
   Limited_Containers_Test;
end Main;