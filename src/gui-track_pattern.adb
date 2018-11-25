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

with Gtk.Grid;      use Gtk.Grid;
with Glib;          use Glib;
with Gtk.Widget;    use Gtk.Widget;

package body GUI.Track_Pattern is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Widget;
                      Track : Track_Id)
   is
   begin
      Self := new Widget_Record;
      Track_Pattern.Initialize (Self, Track);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self  : not null access Widget_Record'Class;
                         Track : Track_Id)
   is
   begin
      Initialize (Parent (Self));

      Self.Track := Track;

      Self.Set_Row_Homogeneous (True);
      Self.Set_Column_Homogeneous (True);

      for Step in Step_Id loop
         Step_Enabled.Gtk_New (Self.Enabled (Step), Track, Step);
         Self.Enabled (Step).Set_Vexpand (True);
         Self.Attach (Self.Enabled (Step), Gint (Step), 0, 1, 1);

         Step_Condition.Gtk_New (Self.Conditions (Step), Track, Step);
         Self.Attach (Self.Conditions (Step), Gint (Step), 1, 1, 1);
      end loop;

   end Initialize;

   ------------
   -- Update --
   ------------

   overriding
   procedure Update (Self : in out Widget_Record) is
   begin
      for Enabled of Self.Enabled loop
         Enabled.Update;
      end loop;

      for Cond of Self.Conditions loop
         Cond.Update;
      end loop;
   end Update;

end GUI.Track_Pattern;
