with Analytical_Engine.Annunciator_Panel;
with Analytical_Engine.Attendant;
with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;
private with Ada.Finalization;
package Analytical_Engine.Store is

   type Column is range 0 .. 999;

   type Instance
     (Panel     : not null Annunciator_Panel.Class_P;
      Attendant : not null Analytical_Engine.Attendant.Instance_P)
   is tagged limited private;
   type Instance_P is access all Instance;

   procedure Set (This : in out Instance; Col : Column; To : Big_Integer);

   procedure Get (This : Instance; Col : Column; Result : out Big_Integer);

private

   type Column_Array is array (Column) of Big_Integer;

   type Instance
     (Panel     : not null Annunciator_Panel.Class_P;
      Attendant : not null Analytical_Engine.Attendant.Instance_P)
     is new Ada.Finalization.Limited_Controlled with record
        Columns : Column_Array;
     end record;

   overriding
   procedure Initialize (This : in out Instance);

end Analytical_Engine.Store;
