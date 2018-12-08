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

with Glib;               use Glib;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.Grid;           use Gtk.Grid;
with Gtk.Window;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Gtk.GEntry;   use Gtk.GEntry;
with Gtk.Button;   use Gtk.Button;
with Gtk.Label;    use Gtk.Label;

with Gtk.Handlers;
with Gtk.Editable;

with Gdk.Event;
with Ada.Text_IO;

with GUI.Sample_Actions;

package body GUI.Sample_Grid_Cell is

   package Entry_Callbacks is new Gtk.Handlers.User_Callback
     (Gtk_Entry_Record,
      Sample_Id);

   function To_Widget is new Widget_From_Child (Widget_Record, Widget);

   function Edit_Sample_Callback
     (W     : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean;
   function Play_Sample_Callback
     (W     : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean;
   function Load_Sample_Callback
     (W     : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean;

   procedure Update_Name (W : access Gtk_Entry_Record'Class;
                          Id : Sample_Id);

   --------------------------
   -- Edit_Sample_Callback --
   --------------------------

   function Edit_Sample_Callback
     (W     : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean
   is
      pragma Unreferenced (Event);
      Self : constant Widget := To_Widget (W);
   begin
      Ada.Text_IO.Put_Line ("Edit sample" & Self.Id'Img);
      GUI.Sample_Actions.Edit (Self.Id,
                               Gtk.Window.Gtk_Window (W.Get_Toplevel));
      return False;
   end Edit_Sample_Callback;

   --------------------------
   -- Play_Sample_Callback --
   --------------------------

   function Play_Sample_Callback
     (W     : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean
   is
      pragma Unreferenced (Event);
      Self : constant Widget := To_Widget (W);
   begin
      Ada.Text_IO.Put_Line ("Play sample" & Self.Id'Img);
      Sample_Actions.Play_Preview (Self.Id);
      return False;
   end Play_Sample_Callback;

   --------------------------
   -- Load_Sample_Callback --
   --------------------------

   function Load_Sample_Callback
     (W     : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean
   is
      pragma Unreferenced (Event);
      Self : constant Widget := To_Widget (W);
   begin
      GUI.Sample_Actions.Load_Sample (Self.Id);
      Self.Update;
      return False;
   end Load_Sample_Callback;

   -----------------
   -- Update_Name --
   -----------------

   procedure Update_Name (W :  access Gtk_Entry_Record'Class;
                          Id : Sample_Id)
   is
      Name : constant String :=
        Head (W.Get_Text,
              Natural (Sample_Manager.Sample_Name_Size'Last),
              ASCII.NUL);
   begin
      Sample_Manager.Set_Name (Id, Name);
   end Update_Name;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Widget;
                      Id   : Sample_Id)
   is
   begin
      Self := new Widget_Record;
      Sample_Grid_Cell.Initialize (Self, Id);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Widget_Record'Class;
                         Id   : Sample_Id)
   is
      Icon_Size : constant := 1;

      L : constant Gtk_Label := Gtk_Label_New (Id'Img);

      Edit : constant Gtk_Button :=
        Gtk_Button_New_From_Icon_Name ("system-run", Icon_Size);
      Play : constant Gtk_Button :=
        Gtk_Button_New_From_Icon_Name ("media-playback-start", Icon_Size);
      Load : constant Gtk_Button :=
        Gtk_Button_New_From_Icon_Name ("document-open", Icon_Size);
   begin

      Parent_Package.Initialize (Parent (Self));
      Self.Id := Id;

      Self.Set_Name ("sample");
      Self.Set_Vexpand (False);
      Self.Set_Hexpand (False);
      Self.Set_Events (Gdk.Event.Pointer_Motion_Mask);

      Self.Name_Entry := Gtk_Entry_New;
      Self.Name_Entry.Set_Name ("sample-name-entry");

      Self.Name_Entry.Set_Max_Length
        (Gint (Sample_Manager.Sample_Name_Size'Last));

      Self.Name_Entry.Set_Text (Sample_Manager.Name (Id));

      Entry_Callbacks.Connect (Self.Name_Entry,
                               Gtk.Editable.Signal_Changed,
                               Update_Name'Access,
                               Id);

      L.Set_Name ("sample-id-label");
      L.Set_Width_Chars (5);
      L.Set_Alignment (0.0, 0.5);

      Edit.Set_Name ("sample-edit-button");
      Play.Set_Name ("sample-play-button");
      Load.Set_Name ("sample-load-button");

      Edit.On_Button_Press_Event (Edit_Sample_Callback'Access);
      Play.On_Button_Press_Event (Play_Sample_Callback'Access);
      Load.On_Button_Press_Event (Load_Sample_Callback'Access);

      Self.Attach (Self.Name_Entry, 0, 0, Width => 4);
      Self.Attach (L, 0, 1);
      Self.Attach (Load, 1, 1);
      Self.Attach (Play, 2, 1);
      Self.Attach (Edit, 3, 1);

      Show_All (Self);

   end Initialize;

   ------------
   -- Update --
   ------------

   overriding
   procedure Update (Self : in out Widget_Record) is
   begin
      Self.Name_Entry.Set_Text (Sample_Manager.Name (Self.Id));
      Self.Queue_Draw;
   end Update;

end GUI.Sample_Grid_Cell;
