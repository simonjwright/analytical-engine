with Analytical_Engine.Framework;

package body Analytical_Engine.Card.Operation_Card is

   procedure Execute
     (C : Operation_Card;
      In_The_Framework : in out Analytical_Engine.Framework.Instance)
   is
   begin
      In_The_Framework.Mill.Set_Operation (C.Op);
   end Execute;

end Analytical_Engine.Card.Operation_Card;
