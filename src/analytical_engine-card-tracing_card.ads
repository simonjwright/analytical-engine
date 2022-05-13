private package Analytical_Engine.Card.Tracing_Card is

   pragma Elaborate_Body;

   type Tracing_Card is new Card with record
      Tracing : Boolean;
   end record;
   overriding
   procedure Execute (C : Tracing_Card;
                      In_The_Framework : in out Framework.Instance);

   subtype Card is Tracing_Card;

end Analytical_Engine.Card.Tracing_Card;
