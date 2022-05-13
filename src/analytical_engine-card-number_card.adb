with Analytical_Engine.Framework;  -- was 'limited with' in parent spec

package body Analytical_Engine.Card.Number_Card is

   procedure Execute
     (C : Number_Card;
      In_The_Framework : in out Analytical_Engine.Framework.Instance)
   is
   begin
      In_The_Framework.Store.Set (Col => C.Target_Column,
                                  To  => C.Value.all);
   end Execute;

end Analytical_Engine.Card.Number_Card;
