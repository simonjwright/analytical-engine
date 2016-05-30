with Analytical_Engine.Annunciator_Panel;
package Analytical_Engine.Output.Printer is

   type Instance (Panel : not null Annunciator_Panel.Class_P)
   is new Analytical_Engine.Output.Instance with private;

   overriding
   procedure Output (To : Instance; S : String);

private

   type Instance (Panel : not null Annunciator_Panel.Class_P)
     is new Analytical_Engine.Output.Instance (Panel => Panel)
     with null record;

   overriding
   procedure Initialize (This : in out Instance);

end Analytical_Engine.Output.Printer;
