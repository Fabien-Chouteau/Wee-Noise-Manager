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
with Gtk.Button;
with Gtk.Enums;  use Gtk.Enums;

with Cairo;         use Cairo;
with Glib;          use Glib;

with Ada.Unchecked_Deallocation;

with Audio_Interface; use Audio_Interface;

package body GUI.Sample_Edit_Dialog is

   use Parent_Package;

   procedure Free is new Ada.Unchecked_Deallocation (Sample_Data,
                                                     Sample_Data_Ptr);

   function To_Widget is new Widget_From_Child (Widget_Record,
                                                Widget);

   function On_Draw (W  : access Gtk_Widget_Record'Class;
                     Cr : Cairo.Cairo_Context)
                     return Boolean;

   procedure On_Gain_Changed (W : access Gtk_Range_Record'Class);
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

      X_Step : constant Gdouble := Width / Gdouble (Frames);
      X, Y : Gdouble := 0.0;

   begin

      Set_Line_Width (Cr, 0.1);

      Set_Source_Rgb (Cr, Red   => 1.0,
                         Green => 1.0,
                         Blue  => 1.0);
      Paint (Cr);

      Set_Source_Rgb (Cr, Red   => 0.0,
                      Green => 0.0,
                      Blue  => 0.0);

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

      return True;
   end On_Draw;

   ---------------------
   -- On_Gain_Changed --
   ---------------------

   procedure On_Gain_Changed (W : access Gtk_Range_Record'Class) is
      Self : constant Widget := To_Widget (W);
   begin
      On_Any_Control_Changed (Self);
   end On_Gain_Changed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self  : out Widget;
                      Id    : Sample_Id)
   is
   begin
      Self := new Widget_Record;
      Sample_Edit_Dialog.Initialize (Self, Id);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Widget_Record'Class;
                         Id   : Sample_Id)
   is
      Unused : Gtk.Widget.Gtk_Widget;

   begin
      Parent_Package.Initialize (Parent (Self));

      -- Dialog --
      Self.Set_Title ("Editing sample" & Id'Img);
      Unused := Self.Add_Button ("Save", Gtk_Response_Apply);
      Unused := Self.Add_Button ("Cancel", Gtk_Response_Cancel);

      Self.Get_Content_Area.Add
        (Gtk.Button.Gtk_Button_New_With_Label ("Play"));

      -- Draw --
      Gtk.Event_Box.Gtk_New (Self.Draw);
      Self.Draw.On_Draw (On_Draw'Access, After => True);
      Self.Draw.Set_Vexpand (True);
      Self.Get_Content_Area.Add (Self.Draw);

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
      Self.Draw.Destroy;
      Destroy (Parent (Self));
   end Destroy;

end GUI.Sample_Edit_Dialog;
