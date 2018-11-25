-------------------------------------------------------------------------------
--                                                                           --
--                             Wee Noise Manager                             --
--                                                                           --
--                     Copyright (C) 2018 Fabien Chouteau                    --
--                                                                           --
--    Wee Noise Manager is free software: you can redistribute it and/or     --
--    modify it under the terms of the GNU General Public License as         --
--    published by the Free Software Foundation, either version 3 of the     --
--    License, or (at your option) any later version.                        --
--                                                                           --
--    Wee Noise Manager is distributed in the hope that it will be useful,   --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU       --
--    General Public License for more details.                               --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with Wee Noise Manager.                                          --
--    If not, see <http://www.gnu.org/licenses/>.                            --
--                                                                           --
-------------------------------------------------------------------------------

with Gtk.Box;    use Gtk.Box;
with Glib;       use Glib;

with Gtk.Enums;

with GUI.Vertical_Gauge;

package body GUI.Params_Page is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self  : out Widget;
                      Track : Track_Id;
                      Param : Param_Id)
   is
   begin
      Self := new Widget_Record;
      Params_Page.Initialize (Self, Track, Param);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self  : not null access Widget_Record'Class;
                         Track : Track_Id;
                         Param : Param_Id)
   is
   begin
      Initialize (Parent (Self),
                  Gtk.Enums.Orientation_Horizontal,
                  Spacing => 0);

      Self.Track := Track;
      Self.Param := Param;

      Self.Set_Homogeneous (True);
      Self.Set_Hexpand (True);
      Self.Set_Vexpand (True);

      for Step in Step_Id loop

         declare
            P : GUI.Vertical_Gauge.Widget;
         begin
            GUI.Vertical_Gauge.Gtk_New (P, Track, Step, Param);
            Self.Steps (Step) := Updatable (P);
            Self.Add (P);
         end;
      end loop;

   end Initialize;

   ------------
   -- Update --
   ------------

   overriding
   procedure Update (Self : in out Widget_Record) is
   begin
      for Step of Self.Steps loop
         Step.Update;
      end loop;
   end Update;

end GUI.Params_Page;
