private package Analytical_Engine.Card.Comment_Card is

   type Comment_Card is new Card with null record;
   overriding
   procedure Execute (C : Comment_Card;
                      In_The_Framework : in out Framework.Instance);

   subtype Card is Comment_Card;

private

   procedure Execute (C : Comment_Card;
                      In_The_Framework : in out Framework.Instance)
     is null;

end Analytical_Engine.Card.Comment_Card;
