with Analytical_Engine.Annunciator_Panel;
with Ada.Finalization;
package Analytical_Engine.Output is

   type Instance (Panel : not null Annunciator_Panel.Class_P)
     is abstract new Ada.Finalization.Limited_Controlled with private;
   type Class_P is access all Instance'Class;

   procedure Output (To : Instance; S : String) is abstract;

private

   type Instance (Panel : not null Annunciator_Panel.Class_P)
     is abstract new Ada.Finalization.Limited_Controlled with null record;

end Analytical_Engine.Output;
