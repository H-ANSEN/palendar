with Interfaces.C; use Interfaces.C;

package Raylib is

    type VoidPtr is access all Integer;

    type Rectangle is record
        x      : Float;
        y      : Float;
        width  : Float;
        height : Float;
    end record with Convention => C_Pass_By_Copy;

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

    type Texture2D is record
        id      : unsigned;
        width   : int;
        height  : int;
        mipmaps : int;
        format  : int;
    end record with Convention => C_Pass_By_Copy;

    type RenderTexture is record
        id      : unsigned;
        texture : Texture2D;
        depth   : Texture2D;
    end record with Convention => C_Pass_By_Copy;

    type Image is record
        data    : VoidPtr;
        width   : int;
        height  : int;
        mipmaps : int;
        format  : int;
    end record with Convention => C_Pass_By_Copy;

    type GlyphInfo is record
        value    : int;
        offsetX  : int;
        offsetY  : int;
        advanceX : int;
        img      : Image;
    end record with Convention => C_Pass_By_Copy;

    type Font is record
        baseSize     : int;
        glyphCount   : int;
        glyphPadding : int;
        texture      : Texture2D;    
        recs         : access Rectangle;
        glyphs       : access GlyphInfo;
    end record with Convention => C_Pass_By_Copy;

    type Camera2D is record
        offset: Vector2;
        target: Vector2;
        rotation: Float;
        zoom: Float;
    end record with Convention => C_Pass_By_Copy;

    type KeyboardKey is (
        KEY_RIGHT,
        KEY_LEFT
    );

    for KeyboardKey use (
        KEY_RIGHT => 262,
        KEY_LEFT  => 263
    ); 
        
    type MouseButton is ( 
        MOUSE_BUTTON_LEFT,
        MOUSE_BUTTON_RIGHT,
        MOUSE_BUTTON_MIDDLE,
        MOUSE_BUTTON_SIDE,
        MOUSE_BUTTON_EXTRA,
        MOUSE_BUTTON_FORWARD,
        MOUSE_BUTTON_BACK
    );
    
    for MouseButton use ( 
        MOUSE_BUTTON_LEFT    => 0,
        MOUSE_BUTTON_RIGHT   => 1,
        MOUSE_BUTTON_MIDDLE  => 2,
        MOUSE_BUTTON_SIDE    => 3,
        MOUSE_BUTTON_EXTRA   => 4,
        MOUSE_BUTTON_FORWARD => 5,
        MOUSE_BUTTON_BACK    => 6 
    );

    type TextureFilter is (
        TEXTURE_FILTER_POINT,
        TEXTURE_FILTER_BILINEAR,
        TEXTURE_FILTER_TRILINEAR,
        TEXTURE_FILTER_ANISOTROPIC_4X,
        TEXTURE_FILTER_ANISOTROPIC_8X,
        TEXTURE_FILTER_ANISOTROPIC_16X
    );

    for TextureFilter use (
        TEXTURE_FILTER_POINT           => 0,
        TEXTURE_FILTER_BILINEAR        => 1,
        TEXTURE_FILTER_TRILINEAR       => 2,
        TEXTURE_FILTER_ANISOTROPIC_4X  => 3,
        TEXTURE_FILTER_ANISOTROPIC_8X  => 4,
        TEXTURE_FILTER_ANISOTROPIC_16X => 5
    );

    type ConfigFlag is (
        FLAG_WINDOW_RESIZABLE
    );

    for ConfigFlag use (
        FLAG_WINDOW_RESIZABLE => 4
    );

    type MouseCursor is (
        MOUSE_CURSOR_DEFAULT,
        MOUSE_CURSOR_POINTING_HAND
    );

    for MouseCursor use (
        MOUSE_CURSOR_DEFAULT       => 0,
        MOUSE_CURSOR_POINTING_HAND => 4
    );

    PINK      : constant Color := (251, 162, 235, 255);
    LIGHTBLUE : constant Color := (130, 170, 251, 255);
    LIGHTGREY : constant Color := (146, 146, 146, 255);
    GRAY      : constant Color := (105, 105, 105, 255);
    DARKGREY  : constant Color := ( 80,  80,  80, 255);
    RED       : constant Color := (211,   0,   0, 255);
    BLUE      : constant Color := (  0,  64, 195, 255);
    WHITE     : constant Color := (255, 255, 255, 255);
    BLACK     : constant Color := (  0,   0,   0, 255);

    procedure InitWindow(width, height: Integer; title: in char_array);
    pragma Import(C, InitWindow, "InitWindow");

    procedure CloseWindow;
    pragma Import(C, CloseWindow, "CloseWindow");

    function WindowShouldClose return C_Bool;
    pragma Import(C, WindowShouldClose, "WindowShouldClose");

    procedure SetWindowMinSize(width, height: Integer);
    pragma Import(C, SetWindowMinSize, "SetWindowMinSize");

    function IsWindowResized return C_Bool;
    pragma Import(C, IsWindowResized, "IsWindowResized");

    function GetScreenWidth return Integer;
    pragma Import(C, GetScreenWidth, "GetScreenWidth");

    function GetScreenHeight return Integer;
    pragma Import(C, GetScreenHeight, "GetScreenHeight");

    procedure SetConfigFlags(flags: unsigned);
    pragma Import(C, SetConfigFlags, "SetConfigFlags");

    procedure ClearBackground(col: Color);
    pragma Import(C, ClearBackground, "ClearBackground");

    procedure BeginDrawing;
    pragma Import(C, BeginDrawing, "BeginDrawing");

    procedure EndDrawing;
    pragma Import(C, EndDrawing, "EndDrawing");

    procedure BeginMode2D(camera: Camera2D);
    pragma Import(C, BeginMode2D, "BeginMode2D");

    procedure EndMode2D;
    pragma Import(C, EndMode2D, "EndMode2D");

    procedure BeginTextureMode(texture: RenderTexture);
    pragma Import(C, BeginTextureMode, "BeginTextureMode");
        
    procedure EndTextureMode;
    pragma Import(C, EndTextureMode, "EndTextureMode");

    procedure SetTargetFPS(fps: int);
    pragma Import(C, SetTargetFPS, "SetTargetFPS");

    function GetFontDefault return Font;
    pragma Import(C, GetFontDefault, "GetFontDefault");

    function LoadFont(fileName: char_array) return Font;
    pragma Import(C, LoadFont, "LoadFont");

    function LoadFontEx(fileName: char_array; fontSize: int; codepoints: access int; codepointCount: int) return Font;
    pragma Import(C, LoadFontEx, "LoadFontEx");

    procedure UnloadFont(fnt: Font);
    pragma Import(C, UnloadFont, "UnloadFont");

    function LoadRenderTexture(width, height: Integer) return RenderTexture;
    pragma Import(C, LoadRenderTexture, "LoadRenderTexture");

    procedure UnloadRenderTexture(texture: RenderTexture);
    pragma Import(C, UnloadRenderTexture, "UnloadRenderTexture");

    procedure DrawTextureRec(texture: Texture2D; rec: Rectangle; pos: Vector2; tint: Color);
    pragma Import(C, DrawTextureRec, "DrawTextureRec");

    procedure DrawText(text: in char_array; posX, posY, fontSize: Integer; col: Color);
    pragma Import(C, DrawText, "DrawText");

    procedure DrawTextEx(fnt: Font; text: char_array; pos: Vector2; fontSize, spacing: Float; col: Color);
    pragma Import(C, DrawTextEx, "DrawTextEx");

    function MeasureTextEx(fnt: Font; text: char_array; fontSize, spacing: Float) return Vector2;
    pragma Import(C, MeasureTextEx, "MeasureTextEx");

    procedure DrawPixelV(position: Vector2; col: Color);
    pragma Import(C, DrawPixelV, "DrawPixelV");

    procedure DrawLineEx(startPos, endPos: Vector2; thick: Float; col: Color);
    pragma Import(C, DrawLineEx, "DrawLineEx");

    procedure DrawRectangle(posX, posY, width, height: Integer; col: Color);
    pragma Import(C, DrawRectangle, "DrawRectangle");

    procedure DrawRectangleRec(rec: Rectangle; col: Color);
    pragma Import(C, DrawRectangleRec, "DrawRectangleRec");

    procedure DrawRectangleLines(posX, posY, width, height: Integer; col: Color);
    pragma Import(C, DrawRectangleLines, "DrawRectangleLines");

    procedure DrawRectangleLinesEx(rec: Rectangle; thick: Float; col: Color);
    pragma Import(C, DrawRectangleLinesEx, "DrawRectangleLinesEx");

    procedure SetTextureFilter(texture: Texture2D; filter: TextureFilter);
    pragma Import(C, SetTextureFilter, "SetTextureFilter");

    function CheckCollisionPointRec(point: Vector2; rec: Rectangle) return C_Bool;
    pragma Import(C, CheckCollisionPointRec, "CheckCollisionPointRec");

    function IsMouseButtonPressed(btn: MouseButton) return C_Bool;
    pragma Import(C, IsMouseButtonPressed, "IsMouseButtonPressed");

    function GetMousePosition return Vector2;
    pragma Import(C, GetMousePosition, "GetMousePosition");

    procedure SetMouseCursor(cursor: MouseCursor);
    pragma Import(C, SetMouseCursor, "SetMouseCursor");

    function IsKeyPressed(key: KeyboardKey) return C_Bool;
    pragma Import(C, IsKeyPressed, "IsKeyPressed");

    procedure DrawCenteredText(text: String; size: Float; fnt: Font; col: Color; rec: Rectangle);
    
end Raylib;
