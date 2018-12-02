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

with Gtk.Enums;     use Gtk.Enums;
with Gtk.Event_Box; use Gtk.Event_Box;
with Gtk.Widget;    use Gtk.Widget;
with Gdk.Event;
with Gtk.Handlers;
with Glib;          use Glib;

package body GUI.Resource_Usage is

   function To_Widget is new Widget_From_Child (Widget_Record, Widget);

   package Event_Box_Callbacks is new Gtk.Handlers.User_Return_Callback
     (Gtk_Event_Box_Record,
      Boolean,
      Element_Id);

   package Box_Callbacks is new Gtk.Handlers.User_Return_Callback
     (Gtk_Box_Record, Boolean, Widget);

   function Mem_Cell_On_Motion (W  : access Gtk_Event_Box_Record'Class;
                                Id : Element_Id)
                                return Boolean;

   function Mem_Cell_On_Leave (W  : access Gtk_Event_Box_Record'Class;
                               Id : Element_Id)
                                return Boolean;

   function On_Draw (W    : access Gtk_Box_Record'Class;
                     Self : Widget)
                     return Boolean;

   ------------------------
   -- Mem_Cell_On_Motion --
   ------------------------

   function Mem_Cell_On_Motion (W  : access Gtk_Event_Box_Record'Class;
                                Id : Element_Id)
                                return Boolean
   is
      Self : constant Widget := To_Widget (W);
   begin
      if Self.Callback /= null then
         Self.Callback (Id, True);
      end if;

      return False;
   end Mem_Cell_On_Motion;

   -----------------------
   -- Mem_Cell_On_Leave --
   -----------------------

   function Mem_Cell_On_Leave (W  : access Gtk_Event_Box_Record'Class;
                               Id : Element_Id)
                               return Boolean
   is
      Self : constant Widget := To_Widget (W);
   begin
      if Self.Callback /= null then
         Self.Callback (Id, False);
      end if;

      return False;
   end Mem_Cell_On_Leave;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Widget)
   is
   begin
      Self := new Widget_Record;
      Initialize (Self);
   end Gtk_New;

   -------------
   -- On_Draw --
   -------------

   function On_Draw (W    : access Gtk_Box_Record'Class;
                     Self : Widget)
                     return Boolean
   is
      Width : constant Gint := W.Get_Allocated_Width;
   begin
      for Id in Element_Id loop
         Self.Cells (Id).Set_Size_Request
           (Gint (Float (Width) * Self.Usage (Id)), -1);

         Self.Cells (Id).Set_Visible (Self.Usage (Id) > 0.0);
      end loop;
      return False;
   end On_Draw;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Widget_Record'Class)
   is
   begin
      Initialize (Parent (Self), Orientation_Vertical, Spacing => 0);

      -- Background --
      Self.BG := Gtk_Event_Box_New;
      Self.BG.Set_Name ("mem-usage-bg");

      Self.Cells_Box := Gtk_Box_New (Orientation_Horizontal, Spacing => 0);

      Self.BG.Add (Self.Cells_Box);
      Self.Add (Self.BG);

      for Id in Element_Id loop
         declare
            Cell : Gtk_Event_Box renames Self.Cells (Id);
         begin

            Cell := Gtk_Event_Box_New;

            Cell.Set_Events (Gdk.Event.Pointer_Motion_Mask);

            Event_Box_Callbacks.Connect (Cell,
                                         "motion-notify-event",
                                         Mem_Cell_On_Motion'Access,
                                         Id);
            Event_Box_Callbacks.Connect (Cell,
                                         "leave-notify-event",
                                         Mem_Cell_On_Leave'Access,
                                         Id);

            Cell.Set_Name ("mem-usage-cell");
            Cell.Set_Size_Request (50, -1);
            Self.Cells_Box.Pack_Start (Cell, False, False, 0);
         end;
      end loop;

      Box_Callbacks.Connect (Self.Cells_Box, "draw", On_Draw'Access, Self);
      Self.Cells_Box.Set_Size_Request (-1, 100);

      Show_All (Self);
   end Initialize;

   ---------------
   -- Set_Usage --
   ---------------

   procedure Set_Usage (Self  : in out Widget_Record;
                        Id    : Element_Id;
                        Usage : Float)
   is
   begin
      Self.Usage (Id) := Usage;
   end Set_Usage;

   ---------------------------
   -- Set_Selected_Callback --
   ---------------------------

   procedure Set_Selected_Callback (Self     : in out Widget_Record;
                                    Callback : Selected_Callback)
   is
   begin
      Self.Callback := Callback;
   end Set_Selected_Callback;

   ------------------
   -- Set_Selected --
   ------------------

   procedure Set_Selected (Self     : in out Widget_Record;
                           Id       : Element_Id;
                           Selected : Boolean)
   is
   begin
      if Selected then
         Self.Cells (Id).Set_Name ("mem-usage-cell-selected");
      else
         Self.Cells (Id).Set_Name ("mem-usage-cell");
      end if;
   end Set_Selected;

end GUI.Resource_Usage;
