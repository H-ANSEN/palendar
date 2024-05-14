with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Raylib is

    function GetCodepoint(text: String; codepointSize: access int) return Integer is
        use Interfaces.C;
        use Interfaces.C.Strings;

        c_text : chars_ptr    := New_String(text);
        result : constant int := GetCodepoint(c_text, codepointSize);
    begin
        Free(c_text);
        return Integer(result);
    end;

    function TextLength(text: String) return Integer is
        use Interfaces.C;
        use Interfaces.C.Strings;

        c_text : chars_ptr := New_String(text);
        result : unsigned  := TextLength(c_text);
    begin
        Free(c_text);
        return Integer(result);
    end;

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
