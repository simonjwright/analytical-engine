with Analytical_Engine.Framework;

package body Analytical_Engine.Card.Stepping_Card is

   procedure Execute (C : Stepping_Card;
                      In_The_Framework : in out Framework.Instance)
   is
   begin
      In_The_Framework.Mill.Step_Axes (Direction => C.Direction,
                                       Amount    => C.Step_Count);
   end Execute;

end Analytical_Engine.Card.Stepping_Card;
