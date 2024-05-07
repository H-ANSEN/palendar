with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Raylib is

    procedure DrawCenteredText(text: String; size: Float; fnt: Font; col: Color; rec: Rectangle) is
        text_size     : Vector2;
        text_draw_pos : Vector2;
    begin
        text_size := MeasureTextEx(fnt, To_C(Trim(text, Both)), size, 1.0);
        text_draw_pos.x := rec.x + (rec.width - text_size.x) / 2.0;
        text_draw_pos.y := rec.y + (rec.height - text_size.y) / 2.0;

        DrawTextEx(fnt, To_C(Trim(text, Both)), text_draw_pos, size, 1.0, col);
    end;

end Raylib;
