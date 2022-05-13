with Analytical_Engine.Framework;

package body Analytical_Engine.Card.Combinatorial_Card is

   procedure Execute (C : Combinatorial_Card;
                      In_The_Framework : in out Framework.Instance)
   is
   begin
      if not C.Conditional or else In_The_Framework.Mill.Run_Up_Set then
         In_The_Framework.Card_Reader.Step
           (if C.Advance then C.Card_Count else -C.Card_Count);
      end if;
   end Execute;

end Analytical_Engine.Card.Combinatorial_Card;
