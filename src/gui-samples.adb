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

with Gtk.Box;       use Gtk.Box;
with Glib;          use Glib;
with Gtk.Widget;    use Gtk.Widget;

with Gtk.Enums;

package body GUI.Samples is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self  : out Widget)
   is
   begin
      Self := new Widget_Record;
      GUI.Samples.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self  : not null access Widget_Record'Class)
   is
   begin
      Initialize (Parent (Self),
                  Gtk.Enums.Orientation_Vertical,
                  Spacing => 0);

      Self.Set_Hexpand (True);
      Self.Set_Vexpand (True);

      Memory_Usage.Gtk_New (Self.Usage);

--        Mem_Usage.Set_Selected_Callback (Set_Selected_Sample'Access);

      Self.Usage.Set_Usage (1, 0.1);
      Self.Usage.Set_Usage (2, 0.15);
      Self.Usage.Set_Usage (3, 0.234);
      Self.Usage.Set_Usage (40, 0.1);
      Self.Usage.Set_Usage (50, 0.2);

      Self.Add (Self.Usage);

      GUI.Sample_Grid.Gtk_New (Self.Grid);
--        Manager.Set_Selected_Callback (Set_Selected_Sample'Access);

      Self.Add (Self.Grid);

   end Initialize;

   ------------
   -- Update --
   ------------

   overriding
   procedure Update (Self : in out Widget_Record) is
   begin
      raise Program_Error;
   end Update;

end GUI.Samples;
