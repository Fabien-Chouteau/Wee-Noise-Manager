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

with Gtk.Widget; use Gtk.Widget;
with Gtk.GRange; use Gtk.GRange;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Button; use Gtk.Button;

with Cairo;         use Cairo;
with Glib;          use Glib;

with Gdk;
with Gdk.Cursor;

with Ada.Unchecked_Deallocation;

with Audio; use Audio;
with Audio_Interface;
with Gdk.Event; use Gdk.Event;
with Gdk.Window;

package body GUI.Sample_Edit_Dialog is

   use Parent_Package;

   procedure Free is new Ada.Unchecked_Deallocation (Sample_Data,
                                                     Sample_Data_Ptr);

   function To_Widget is new Widget_From_Child (Widget_Record,
                                                Widget);

   function On_Draw (W  : access Gtk_Widget_Record'Class;
                     Cr : Cairo.Cairo_Context)
                     return Boolean;

   function On_Motion (W     : access Gtk_Widget_Record'Class;
                       Event : Gdk.Event.Gdk_Event_Motion)
                       return Boolean;

   function On_Button_Press (W     : access Gtk_Widget_Record'Class;
                             Event : Gdk.Event.Gdk_Event_Button)
                             return Boolean;

   function On_Button_Release (W     : access Gtk_Widget_Record'Class;
                               Event : Gdk.Event.Gdk_Event_Button)
                               return Boolean;

   procedure On_Gain_Changed (W : access Gtk_Range_Record'Class);
   procedure On_Play (W : access Gtk_Button_Record'Class);
   procedure On_Any_Control_Changed (Self : not null Widget);

   ----------------------------
   -- On_Any_Control_Changed --
   ----------------------------

   procedure On_Any_Control_Changed (Self : not null Widget) is
      Gain  : constant Gdouble := Self.Gain.Get_Value;
      Frame : Mono_Frame;
   begin
      for Block_Id in Self.After.all'Range loop
         for Frame_Id in Sample_Block_Size loop
            Frame := Self.Before (Block_Id) (Frame_Id);

            Frame := Mono_Frame (Gdouble (Frame) * Gain);
            Self.After (Block_Id) (Frame_Id) := Frame;
         end loop;
      end loop;

      Self.Draw.Queue_Draw;
   end On_Any_Control_Changed;

   -------------
   -- On_Draw --
   -------------

   function On_Draw (W  : access Gtk_Widget_Record'Class;
                     Cr : Cairo.Cairo_Context)
                     return Boolean
   is
      Width  : constant Gdouble := Gdouble (W.Get_Allocated_Width);
      Height : constant Gdouble := Gdouble (W.Get_Allocated_Height);
      Self   : constant Widget := To_Widget (W);

      Frames : constant Natural :=
        Sample_Manager.Sample_Block_Size'Last * Self.After'Length;

      Block_Width : constant Gdouble := Width / Gdouble (Self.Before'Length);
      X_Step : constant Gdouble := Width / Gdouble (Frames);
      X, Y : Gdouble := 0.0;

   begin

      Set_Line_Width (Cr, 0.1);

      Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
      Paint (Cr);

      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);

      Move_To (Cr, 0.0, Height * 0.5);

      for Block of Self.After.all loop
         for Frame of Block loop

            Y := (Gdouble (Frame) / Gdouble (Mono_Frame'Last)); -- -1.0 .. 1.0
            Y := Y / 2.0; -- -0.5 .. 0.5
            Y := Y + 0.5; -- 0.0 .. 1.0
            Y := Y * Height; -- 0.0 .. Height

            Line_To (Cr, X, Y);

            X := X + X_Step;
         end loop;
      end loop;
      Stroke (Cr);

      X := 0.0;
      for Block of Self.After.all loop
         X := X + Width / Gdouble (Self.After'Length);
         Move_To (Cr, X, 0.0);
         Line_To (Cr, X, Height);
         Stroke (Cr);
      end loop;

      --  Disabled blocks (before first and after last)
      Set_Source_Rgba (Cr, 0.0, 0.0, 0.0, 0.5);

      Rectangle (Cr     => Cr,
                 X      => 0.0,
                 Y      => 0.0,
                 Width  => Block_Width * (Gdouble (Self.First_Block) - 1.0),
                 Height => Height);

      Rectangle (Cr     => Cr,
                 X      => Block_Width * Gdouble (Self.Last_Block),
                 Y      => 0.0,
                 Width  => Width,
                 Height => Height);
      Cairo.Fill (Cr);

      --  First and last block handles
      Set_Source_Rgba (Cr, 0.0, 1.0, 0.0, 1.0);
      Set_Line_Width (Cr, 5.0);
      Move_To (Cr,
               Block_Width * (Gdouble (Self.First_Block) - 1.0),
               0.0);
      Line_To (Cr,
               Block_Width * (Gdouble (Self.First_Block) - 1.0),
               Height);
      Stroke (Cr);

      Set_Source_Rgba (Cr, 1.0, 0.0, 0.0, 1.0);
      Move_To (Cr,
               Block_Width * Gdouble (Self.Last_Block),
               0.0);
      Line_To (Cr,
               Block_Width * Gdouble (Self.Last_Block),
               Height);
      Stroke (Cr);
      return True;
   end On_Draw;

   ---------------
   -- On_Motion --
   ---------------

   function On_Motion (W  : access Gtk_Widget_Record'Class;
                       Event : Gdk.Event.Gdk_Event_Motion)
                       return Boolean
   is
      Self : constant Widget := To_Widget (W);
      Cursor : Gdk.Gdk_Cursor;

      Width       : constant Gdouble :=
        Gdouble (Self.Draw.Get_Allocated_Width);
      Block_Width : constant Gdouble :=  Width / Gdouble (Self.Before'Length);

      Drag_Target : Sample_Block_Id;
   begin
      if Self.Dragging /= None then
         Cursor := Gdk.Cursor.Gdk_Cursor_New_From_Name
           (W.Get_Display, "grabbing");

         Drag_Target := Sample_Block_Id ((Event.X / Block_Width) + 1.0);

         case Self.Dragging is
            when First =>
               if Drag_Target in Self.Before'First .. Self.Last_Block then
                  Self.First_Block := Drag_Target;
               end if;
            when Last =>
               if Drag_Target in Self.First_Block .. Self.Before'Last then
                  Self.Last_Block := Drag_Target;
               end if;
            when others =>
               raise Program_Error with "Unreachable";
         end case;

         W.Queue_Draw;

      elsif Self.Cursor_On_First_Block (Event.X)
        or else
          Self.Cursor_On_Last_Block (Event.X)
      then
         Cursor := Gdk.Cursor.Gdk_Cursor_New_From_Name
           (W.Get_Display, "grab");
      else
         Cursor := Gdk.Cursor.Gdk_Cursor_New_From_Name
           (W.Get_Display, "default");
      end if;

      Gdk.Window.Set_Cursor (W.Get_Window, Cursor);
      return False;
   end On_Motion;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press (W     : access Gtk_Widget_Record'Class;
                             Event : Gdk.Event.Gdk_Event_Button)
                             return Boolean
   is
      Self : constant Widget := To_Widget (W);
   begin
      if Self.Dragging /= None then
         raise Program_Error with "We are already dragging...";
      elsif Self.Cursor_On_First_Block (Event.X) then
         Self.Dragging := First;
      elsif Self.Cursor_On_Last_Block (Event.X) then
         Self.Dragging := Last;
      end if;
      return False;
   end On_Button_Press;

   -----------------------
   -- On_Button_Release --
   -----------------------

   function On_Button_Release (W     : access Gtk_Widget_Record'Class;
                               Event : Gdk.Event.Gdk_Event_Button)
                               return Boolean
   is
      pragma Unreferenced (Event);
      Self : constant Widget := To_Widget (W);
   begin
      Self.Dragging := None;
      return False;
   end On_Button_Release;

   ---------------------
   -- On_Gain_Changed --
   ---------------------

   procedure On_Gain_Changed (W : access Gtk_Range_Record'Class) is
      Self : constant Widget := To_Widget (W);
   begin
      On_Any_Control_Changed (Self);
   end On_Gain_Changed;

   -------------
   -- On_Play --
   -------------

   procedure On_Play (W : access Gtk_Button_Record'Class) is
      Self : constant Widget := To_Widget (W);
   begin
      Audio_Interface.Sample_Preview_Flush;
      for Index in Self.First_Block .. Self.Last_Block loop
         Audio_Interface.Push_Sample_Preview (Self.After (Index));
      end loop;
   end On_Play;

   ---------------------------
   -- Cursor_On_First_Block --
   ---------------------------

   function Cursor_On_First_Block (Self : Widget_Record;
                                   X    : Glib.Gdouble)
                                   return Boolean
   is
      Width       : constant Gdouble :=
        Gdouble (Self.Draw.Get_Allocated_Width);
      Block_Width : constant Gdouble := Width / Gdouble (Self.Before'Length);
      From        : constant Gdouble :=
        (Gdouble (Self.First_Block) - 1.5) * Block_Width;
      To          : constant Gdouble := From + Block_Width;
   begin
      return X in From .. To;
   end Cursor_On_First_Block;

   ----------------------
   -- Cursor_On__Block --
   ----------------------

   function Cursor_On_Last_Block (Self : Widget_Record;
                                   X    : Glib.Gdouble)
                                   return Boolean
   is
      Width       : constant Gdouble :=
        Gdouble (Self.Draw.Get_Allocated_Width);
      Block_Width : constant Gdouble := Width / Gdouble (Self.Before'Length);
      From        : constant Gdouble :=
        (Gdouble (Self.Last_Block) - 0.5) * Block_Width;
      To          : constant Gdouble := From + Block_Width;
   begin
      return X in From .. To;
   end Cursor_On_Last_Block;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self          : out Widget;
                      Parent_Window : Gtk.Window.Gtk_Window;
                      Id            : Sample_Id)
   is
   begin
      Self := new Widget_Record;
      Sample_Edit_Dialog.Initialize (Self, Parent_Window, Id);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self          : not null access Widget_Record'Class;
                         Parent_Window : Gtk.Window.Gtk_Window;
                         Id            : Sample_Id)
   is
      pragma Unreferenced (Parent_Window);
      Unused : Gtk.Widget.Gtk_Widget;

   begin
      Parent_Package.Initialize (Parent (Self),
                                 "Editing sample" & Id'Img,
                                 null, --  Parent_Window,
                                 Flags => 0);

      -- Dialog --
      Self.Set_Resizable (True);
      Unused := Self.Add_Button ("Save", Gtk_Response_Apply);
      Unused := Self.Add_Button ("Cancel", Gtk_Response_Cancel);

      Self.Play := Gtk.Button.Gtk_Button_New_With_Label ("Play");
      Self.Play.On_Clicked (On_Play'Access, After => True);
      Self.Get_Content_Area.Add (Self.Play);

      -- Draw --
      Gtk.Event_Box.Gtk_New (Self.Draw);
      Self.Draw.On_Draw (On_Draw'Access, After => True);
      Self.Draw.Set_Vexpand (True);
      Self.Draw.Set_Events (Gdk.Event.Pointer_Motion_Mask
                            or
                              Gdk.Event.Button_Press_Mask
                            or
                              Gdk.Event.Button_Release_Mask
                           );

      Self.Draw.On_Motion_Notify_Event (On_Motion'Access);
      Self.Draw.On_Button_Press_Event (On_Button_Press'Access);
      Self.Draw.On_Button_Release_Event (On_Button_Release'Access);
      Self.Get_Content_Area.Add (Self.Draw);

      Self.Draw.Set_Vexpand (True);

      -- Gain --
      Gtk.Scale.Gtk_New_With_Range (Self.Gain,
                                    Orientation => Orientation_Horizontal,
                                    Min         => 0.0,
                                    Max         => 1.0,
                                    Step        => 0.01);
      Self.Gain.Set_Value (1.0);
      Self.Gain.On_Value_Changed (On_Gain_Changed'Access, After => True);

      Self.Get_Content_Area.Add (Self.Gain);

      Self.Before := new Sample_Data (1 .. Sample_Manager.Size (Id));
      Self.After := new Sample_Data (1 .. Sample_Manager.Size (Id));

      for Index in 1 .. Sample_Manager.Size (Id) loop
         exit when not Sample_Manager.Read_Block (Id,
                                                  Sample_Block_Id (Index),
                                                  Self.Before (Index));
         Self.After (Index) := Self.Before (Index);
      end loop;

      Self.First_Block := Sample_Block_Id (Self.Before'First);
      Self.Last_Block := Sample_Block_Id (Self.Before'Last);

      Self.Show_All;
   end Initialize;

   -------------
   -- Destroy --
   -------------

   overriding
   procedure Destroy (Self : not null access Widget_Record)
   is
   begin
      Free (Self.Before);
      Free (Self.After);
      --  Destroy (Parent (Self));
   end Destroy;

   ---------------------
   -- Modified_Sample --
   ---------------------

   function Modified_Sample (Self : Widget_Record)
                             return not null Sample_Data_Ptr
   is (Self.After);

end GUI.Sample_Edit_Dialog;
