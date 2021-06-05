with GNATCOLL.GMP.Integers;

with Analytical_Engine.Store;

private package Analytical_Engine.Card.Number_Card is

   pragma SPARK_Mode (On);
   pragma Elaborate_Body;

   type Big_Integer_P is access GNATCOLL.GMP.Integers.Big_Integer;
   --  because Big_Integer is limited

   type Number_Card is new Card with record
      Target_Column : Store.Column;
      Value         : Big_Integer_P;
   end record;
   overriding
   procedure Execute (C : Number_Card;
                      In_The_Framework : in out Framework.Instance);

   subtype Card is Number_Card;

end Analytical_Engine.Card.Number_Card;
