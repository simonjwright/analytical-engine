with Analytical_Engine.Mill;
with Analytical_Engine.Store;

private package Analytical_Engine.Card.Variable_Card is

   pragma SPARK_Mode (On);
   pragma Elaborate_Body;

   type Variable_Card is new Card with record
      Axis     : Mill.Axis; -- Ingress axes are to mill, egress to store
      Column   : Store.Column;
      Preserve : Boolean;   -- If false and the value is being sent to
                            -- the mill, the source column is reset to
                            -- zero after the value has been
                            -- retrieved.
   end record;
   overriding
   procedure Execute (C : Variable_Card;
                      In_The_Framework : in out Framework.Instance);

   subtype Card is Variable_Card;

end Analytical_Engine.Card.Variable_Card;
