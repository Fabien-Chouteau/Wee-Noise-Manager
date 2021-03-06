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

with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Grid;     use Gtk.Grid;

with Glib; use Glib;
with Gtk.Widget; use Gtk.Widget;

with Sample_Manager; use Sample_Manager;

package body GUI.Sample_Grid is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Widget)
   is
   begin
      Self := new Widget_Record;
      Sample_Grid.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Widget_Record'Class)
   is
   begin
      Initialize (Parent (Self));

      Self.Set_Name ("sample-manager");
      Self.Set_Vexpand (True);
      Self.Set_Hexpand (True);

      Self.Cells_Container := Gtk_Grid_New;
      Self.Cells_Container.Set_Column_Spacing (0);
      Self.Cells_Container.Set_Row_Spacing (0);
      Self.Cells_Container.Set_Column_Homogeneous (True);
      Self.Cells_Container.Set_Row_Homogeneous (True);

      Self.Add (Self.Cells_Container);

      for Id in Sample_Id loop
         GUI.Sample_Grid_Cell.Gtk_New (Self.Cells (Id), Id);
         Self.Cells_Container.Attach (Self.Cells (Id),
                                      Gint (Id) mod 10,
                                      Gint (Id) / 10);
      end loop;

      Self.Show_All;
   end Initialize;

end GUI.Sample_Grid;
