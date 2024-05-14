with Raylib; use Raylib;

with Interfaces.C; use Interfaces.C;
with System; use System;

package body Notepad is

-- Forward Decl ----------------------------------------------------------------

    procedure DrawTextBoxed(fnt: Font; fnt_size, spacing: Float; text: String; box: Rectangle);

-- Public ----------------------------------------------------------------------

    overriding procedure Draw(self: in out Notepad_T) is

        str : String := "This is a" & Character'Val(10) & "test string to print";
        bounds : Rectangle := (10.0, 10.0, GetMousePosition.x, GetMousePosition.y);

        fnt_scale : constant Float := self.fnt_size / Float(self.fnt.basesize);
        offsetY   : constant Float := Float(self.fnt.basesize) * fnt_scale + 2.0;
    begin

        for i in 1..(Integer(bounds.height / offsetY) - 1) loop
            DrawLineEx((bounds.x, bounds.y + (Float(i) * offsetY)), (bounds.x + bounds.width, bounds.y + (Float(i) * offsetY)), 2.0, LIGHTGREY);
        end loop;

        DrawRectangleLinesEx(bounds, 2.0, BLACK);
        DrawTextBoxed(self.fnt, self.fnt_size, 2.0, str, bounds);


    end;

    overriding procedure Update(self: in out Notepad_T) is
    begin
        null;
    end;

-- Drawing Helpers -------------------------------------------------------------

    procedure DrawTextBoxed(fnt: Font; fnt_size, spacing: Float; text: String; box: Rectangle) is

        -- Map glyph array to Ada array, import to prevent warning
        glyphs : array (0..fnt.glyphCount) of GlyphInfo;
        for glyphs'address use fnt.glyphs;
        pragma Volatile(glyphs);
        pragma Import(Ada, glyphs);

        -- Map bounding rectangle array to Ada array
        recs : array (0..fnt.glyphCount) of Rectangle;
        for recs'address use fnt.recs;
        pragma Volatile(recs);

        length      : constant Integer := TextLength(text);
        fnt_scale   : constant Float   := fnt_size / Float(fnt.basesize);
        line_height : constant Float   := Float(fnt.basesize) * fnt_scale;

        offsetX : Float := 0.0;
        offsetY : Float := 0.0;

    begin

        for i in 1..length loop
            declare

                codepoint_bytes : aliased int := 0;
                codepoint       : Integer     := GetCodepoint(text(i..i), codepoint_bytes'Access);
                index           : int         := int(GetGlyphIndex(fnt, codepoint));
                glyphwidth      : Float;


            begin

                if codepoint = LINE_FEED then

                    offsetY := offsetY + line_height;
                    offsetX := 0.0;
               
                else

                    glyphwidth := (if glyphs(index).advanceX = 0 then recs(index).width * fnt_scale
                                                                 else Float(glyphs(index).advanceX) * fnt_scale);

                    if offsetX + glyphwidth > box.width then
                        offsetY := offsetY + line_height;
                        offsetX := 0.0;
                    end if;

                    -- TODO rather than exit maybe enable some sort of scroll 
                    exit when (offsetY + line_height) > box.height;

                    DrawTextCodepoint(fnt, codepoint, (offsetX + box.x, offsetY + box.y), fnt_size, BLACK);
                    offsetX := offsetX + glyphwidth + spacing;

                end if;

            end; 
        end loop;

    end;

end Notepad;
