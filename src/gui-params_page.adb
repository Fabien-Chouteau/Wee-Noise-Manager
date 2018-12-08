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

with Gtk.Box;            use Gtk.Box;
with Glib;               use Glib;
with Gtk.Enums;
with GUI.Vertical_Gauge;
with GUI.Pitch_Select;
with GUI.Empty_Param;

with Engine_Manager;     use Engine_Manager;

package body GUI.Params_Page is

   function Create_Param_Widget (Track : Track_Id;
                                 Step  : Step_Id;
                                 Param : Param_Id;
                                 Kind  : Parameter_Kind)
                                 return Gtk.Widget.Gtk_Widget;

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
         Self.Steps (Step)  := Create_Param_Widget (Self.Track,
                                                    Step,
                                                    Self.Param,
                                                    None);
         Self.Updatable (Step) := Updatable (Self.Steps (Step));
         Self.Add (Self.Steps (Step));
      end loop;

   end Initialize;

   ------------
   -- Update --
   ------------

   overriding
   procedure Update (Self : in out Widget_Record) is
   begin
      for Step of Self.Updatable loop
         Step.Update;
      end loop;
   end Update;

   -------------------------
   -- Create_Param_Widget --
   -------------------------

   function Create_Param_Widget (Track : Track_Id;
                                 Step  : Step_Id;
                                 Param : Param_Id;
                                 Kind  : Parameter_Kind)
                                 return Gtk.Widget.Gtk_Widget
   is
   begin
      case Kind is
         when Volume | Pan =>
            declare
               P : GUI.Vertical_Gauge.Widget;
            begin
               GUI.Vertical_Gauge.Gtk_New (P, Track, Step, Param,
                                           Fill => Kind in Volume);
               return Gtk.Widget.Gtk_Widget (P);
            end;

         when Pitch =>
            declare
               P : GUI.Pitch_Select.Widget;
            begin
               GUI.Pitch_Select.Gtk_New (P, Track, Step, Param);
               return Gtk.Widget.Gtk_Widget (P);
            end;

         when others =>
            declare
               P : GUI.Empty_Param.Widget;
            begin
               GUI.Empty_Param.Gtk_New (P);
               return Gtk.Widget.Gtk_Widget (P);
            end;
      end case;
   end Create_Param_Widget;

   -----------------
   -- Reconfigure --
   -----------------

   overriding
   procedure Reconfigure (Self : in out Widget_Record) is
      Engine : constant Track_Engine_Kind   := Current_Engine (Self.Track);
      Sub    : constant Track_Sub_Engine_Id := Current_Sub (Self.Track);
      Kind   : constant Parameter_Kind := Param_Kind (Engine, Sub, Self.Param);
   begin
      for Step in Step_Id loop
         Self.Steps (Step).Destroy; --  Also removes it from the container
         Self.Steps (Step) := Create_Param_Widget (Self.Track,
                                                   Step,
                                                   Self.Param,
                                                   Kind);
         Self.Updatable (Step) := Updatable (Self.Steps (Step));
         Self.Add (Self.Steps (Step));
      end loop;

      Self.Show_All;
   end Reconfigure;

end GUI.Params_Page;
