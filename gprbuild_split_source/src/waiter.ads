-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Real_Time;

package Waiter is
  subtype Time_Span is Real_Time.Time_Span; -- Time_Span definition TBD...

  procedure Wait (Period : Time_Span) is null;
end Waiter;