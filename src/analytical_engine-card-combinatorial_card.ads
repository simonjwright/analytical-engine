private package Analytical_Engine.Card.Combinatorial_Card is

   pragma SPARK_Mode (On);
   pragma Elaborate_Body;

   type Combinatorial_Card is new Card with record
      Advance     : Boolean;
      Conditional : Boolean;
      Card_Count  : Positive;
   end record;
   overriding
   procedure Execute (C : Combinatorial_Card;
                      In_The_Framework : in out Framework.Instance);

   subtype Card is Combinatorial_Card;

end Analytical_Engine.Card.Combinatorial_Card;
