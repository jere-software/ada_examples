with Real_Time;

package Waiter is
  subtype Time_Span is Real_Time.Time_Span; -- Time_Span definition TBD...

  procedure Wait (Period : Time_Span) is null;
end Waiter;