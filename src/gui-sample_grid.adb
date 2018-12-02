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

with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Flow_Box; use Gtk.Flow_Box;
with Gtk.Grid;     use Gtk.Grid;
with Gtk.GEntry;   use Gtk.GEntry;
with Gtk.Button;   use Gtk.Button;
with Gtk.Label;    use Gtk.Label;
with Gtk.Handlers;
with Gtk.Editable;

with Gdk.Event;
with Glib; use Glib;
with Gtk.Widget; use Gtk.Widget;

with Sample_Manager; use Sample_Manager;

with GUI.Sample_Actions;

package body GUI.Sample_Grid is

   function Create_Cell (Id : Sample_Id) return Gtk_Grid;

   package Button_Callbacks is new Gtk.Handlers.User_Callback
     (Gtk_Button_Record,
      Sample_Id);

   package Entry_Callbacks is new Gtk.Handlers.User_Callback
     (Gtk_Entry_Record,
      Sample_Id);

   package Grid_Callbacks is new Gtk.Handlers.User_Return_Callback
     (Gtk_Grid_Record,
      Boolean,
      Sample_Id);

   function To_Widget is new Widget_From_Child (Widget_Record, Widget);

   procedure Edit_Sample_Callback (W  : access Gtk_Button_Record'Class;
                                   Id : Sample_Id);
   procedure Play_Sample_Callback (W  : access Gtk_Button_Record'Class;
                                   Id : Sample_Id);
   procedure Load_Sample_Callback (W  : access Gtk_Button_Record'Class;
                                   Id : Sample_Id);

   function Cell_On_Motion (W  : access Gtk_Grid_Record'Class;
                            Id : Sample_Id)
                            return Boolean;

   function Cell_On_Leave (W  : access Gtk_Grid_Record'Class;
                           Id : Sample_Id)
                           return Boolean;

   procedure Update_Name (W : access Gtk_Entry_Record'Class;
                          Id : Sample_Id);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Widget)
   is
   begin
      Self := new Widget_Record;
      Sample_Grid.Initialize (Self);
   end Gtk_New;

   --------------------------
   -- Edit_Sample_Callback --
   --------------------------

   procedure Edit_Sample_Callback (W  : access Gtk_Button_Record'Class;
                                   Id : Sample_Id)
   is
      pragma Unreferenced (W);
   begin
      Ada.Text_IO.Put_Line ("Edit sample" & Id'Img);
   end Edit_Sample_Callback;

   --------------------------
   -- Play_Sample_Callback --
   --------------------------

   procedure Play_Sample_Callback (W  : access Gtk_Button_Record'Class;
                                   Id : Sample_Id)
   is
      pragma Unreferenced (W);
      Block : Sample_Manager.Sample_Block;
      Block_Id : Sample_Manager.Sample_Block_Id :=
        Sample_Manager.Sample_Block_Id'First;
   begin
      Ada.Text_IO.Put_Line ("Play sample" & Id'Img);

      while Sample_Manager.Read_Block (Id, Block_Id, Block) loop
         Ada.Text_IO.Put_Line ("Sample Block");
         Block_Id := Block_Id + 1;
      end loop;
   end Play_Sample_Callback;

   --------------------------
   -- Load_Sample_Callback --
   --------------------------

   procedure Load_Sample_Callback (W  : access Gtk_Button_Record'Class;
                                   Id : Sample_Id)
   is
   begin
      GUI.Sample_Actions.Load_Sample (Id, Gtk.Widget.Gtk_Widget (W));
   end Load_Sample_Callback;

   --------------------
   -- Cell_On_Motion --
   --------------------

   function Cell_On_Motion (W  : access Gtk_Grid_Record'Class;
                            Id : Sample_Id)
                            return Boolean
   is
      Self : constant Widget := To_Widget (W);
   begin
      if Self.Callback /= null then
         Self.Callback (Id, True);
      end if;

      return False;
   end Cell_On_Motion;

   -------------------
   -- Cell_On_Leave --
   -------------------

   function Cell_On_Leave (W  : access Gtk_Grid_Record'Class;
                           Id : Sample_Id)
                           return Boolean
   is
      Self : constant Widget := To_Widget (W);
   begin
      if Self.Callback /= null then
         Self.Callback (Id, False);
      end if;

      return False;
   end Cell_On_Leave;

   -----------------
   -- Update_Name --
   -----------------

   procedure Update_Name (W :  access Gtk_Entry_Record'Class;
                          Id : Sample_Id)
   is
      Name : constant String :=
        Tail (W.Get_Text,
              Natural (Sample_Manager.Sample_Name_Size'Last),
              ASCII.NUL);
   begin
      Sample_Manager.Set_Name (Id, Name);
   end Update_Name;

   -----------------
   -- Create_Cell --
   -----------------

   function Create_Cell (Id : Sample_Id)
                         return Gtk_Grid
   is
      Icon_Size : constant := 1;

      Cell : constant Gtk_Grid := Gtk_Grid_New;
      E : constant Gtk_Entry := Gtk_Entry_New;
      L : constant Gtk_Label := Gtk_Label_New (Id'Img);

      Edit : constant Gtk_Button :=
        Gtk_Button_New_From_Icon_Name ("system-run", Icon_Size);
      Play : constant Gtk_Button :=
        Gtk_Button_New_From_Icon_Name ("media-playback-start", Icon_Size);
      Load : constant Gtk_Button :=
        Gtk_Button_New_From_Icon_Name ("document-open", Icon_Size);
   begin

      Cell.Set_Name ("sample");
      Cell.Set_Events (Gdk.Event.Pointer_Motion_Mask);

      Grid_Callbacks.Connect (Cell,
                              "motion-notify-event",
                              Cell_On_Motion'Access,
                              Id);
      Grid_Callbacks.Connect (Cell,
                              "leave-notify-event",
                              Cell_On_Leave'Access,
                              Id);

      E.Set_Name ("sample-name-entry");

      E.Set_Max_Length (Gint (Sample_Manager.Sample_Name_Size'Last));
      E.Set_Text (Sample_Manager.Name (Id));

      Entry_Callbacks.Connect (E,
                               Gtk.Editable.Signal_Changed,
                               Update_Name'Access,
                               Id);

      L.Set_Name ("sample-id-label");
      L.Set_Width_Chars (5);
      L.Set_Alignment (0.0, 0.5);

      Edit.Set_Name ("sample-edit-button");
      Play.Set_Name ("sample-play-button");
      Load.Set_Name ("sample-load-button");

      Button_Callbacks.Connect (Edit,
                                Gtk.Button.Signal_Clicked,
                                Edit_Sample_Callback'Access,
                                Id);
      Button_Callbacks.Connect (Play,
                                Gtk.Button.Signal_Clicked,
                                Play_Sample_Callback'Access,
                                Id);
      Button_Callbacks.Connect (Load,
                                Gtk.Button.Signal_Clicked,
                                Load_Sample_Callback'Access,
                                Id);

      Cell.Attach (E, 0, 0, Width => 4);
      Cell.Attach (L, 0, 1);
      Cell.Attach (Load, 1, 1);
      Cell.Attach (Play, 2, 1);
      Cell.Attach (Edit, 3, 1);

      Show_All (Cell);
      return Cell;
   end Create_Cell;

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

      Self.Cells_Container := Gtk_Flow_Box_New;
      Self.Cells_Container.Set_Column_Spacing (0);
      Self.Cells_Container.Set_Row_Spacing (0);
      Self.Cells_Container.Set_Homogeneous (True);
      Self.Cells_Container.Set_Max_Children_Per_Line (10);

      Self.Add (Self.Cells_Container);

      for Id in Sample_Id loop
         Self.Cells (Id) := Create_Cell (Id);
         Self.Cells_Container.Add (Self.Cells (Id));
      end loop;

      Self.Show_All;
   end Initialize;

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
                           Id       : Sample_Id;
                           Selected : Boolean)
   is
   begin
      if Selected then
         Self.Cells (Id).Set_Name ("sample-selected");
      else
         Self.Cells (Id).Set_Name ("sample");
      end if;
   end Set_Selected;

end GUI.Sample_Grid;
