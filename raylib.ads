with Interfaces.C; use Interfaces.C;

package Raylib is

    type Vector2 is record
        x: Float;
        y: Float;
    end record with Convention => C_Pass_By_Copy;

    type Color is record
        r: unsigned_char;
        g: unsigned_char;
        b: unsigned_char;
        a: unsigned_char;
    end record with Convention => C_Pass_By_Copy;

    type Camera2D is record
        offset: Vector2;
        target: Vector2;
        rotation: Float;
        zoom: Float;
    end record with Convention => C_Pass_By_Copy;

    RED   : constant Color := (255,   0,   0, 255);
    WHITE : constant Color := (255, 255, 255, 255);
    BLACK : constant Color := (  0,   0,   0, 255);

    procedure InitWindow(width, height: Integer; title: in char_array) with
        Import => True,
        Convention => C,
        External_Name => "InitWindow";

    procedure CloseWindow with
        Import => True,
        Convention => C,
        External_Name => "CloseWindow";

    function WindowShouldClose return C_Bool with
        Import => True,
        Convention => C,
        External_Name => "WindowShouldClose";

    procedure ClearBackground(col: Color) with
        Import => True,
        Convention => C,
        External_Name => "ClearBackground";

    procedure BeginDrawing with
        Import => True,
        Convention => C,
        External_Name => "BeginDrawing";

    procedure EndDrawing with
        Import => True,
        Convention => C,
        External_Name => "EndDrawing";

    procedure BeginMode2D(camera: Camera2D) with
        Import => True,
        Convention => C,
        External_Name => "BeginMode2D";

    procedure EndMode2D with
        Import => True,
        Convention => C,
        External_Name => "EndMode2D";

    procedure SetTargetFPS(fps: int) with
        Import => True,
        Convention => C,
        External_Name => "SetTargetFPS";

    procedure DrawText(text: in char_array; posX, posY, fontSize: Integer; col: Color) with
        Import => True,
        Convention => C,
        External_Name => "DrawText";

    procedure DrawPixelV(position: Vector2; col: Color) with
        Import => True,
        Convention => C,
        External_Name => "DrawPixelV";

    procedure DrawLineEx(startPos, endPos: Vector2; thick: Float; col: Color) with
        Import => True,
        Convention => C,
        External_Name => "DrawLineEx";

    procedure DrawRectangle(posX, posY, width, height: Integer; col: Color) with
        Import => True,
        Convention => C,
        External_Name => "DrawRectangle";

end Raylib;
