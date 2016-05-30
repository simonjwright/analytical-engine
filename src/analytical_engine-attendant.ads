with Analytical_Engine.Annunciator_Panel;
private with Ada.Finalization;
package Analytical_Engine.Attendant is

   type Instance (Panel : not null Annunciator_Panel.Class_P)
   is tagged limited private;

   type Instance_P is access all Instance;

private

   type Instance (Panel : not null Annunciator_Panel.Class_P)
     is new Ada.Finalization.Limited_Controlled with null record;

   overriding
   procedure Initialize (This : in out Instance);

end Analytical_Engine.Attendant;
