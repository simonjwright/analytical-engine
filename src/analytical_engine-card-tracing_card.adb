with Analytical_Engine.Framework;

package body Analytical_Engine.Card.Tracing_Card is

   pragma SPARK_Mode (On);

   procedure Execute (C : Tracing_Card;
                      In_The_Framework : in out Framework.Instance)
   is
   begin
      In_The_Framework.Panel.Set_Tracing (To => C.Tracing);
   end Execute;

end Analytical_Engine.Card.Tracing_Card;
