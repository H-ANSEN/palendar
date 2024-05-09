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

    PINK      : constant Color := (251, 162, 235, 255);
    LIGHTBLUE : constant Color := (130, 170, 251, 255);
    LIGHTGREY : constant Color := (146, 146, 146, 255);
    GRAY      : constant Color := (105, 105, 105, 255);
    DARKGREY  : constant Color := ( 80,  80,  80, 255);
    RED       : constant Color := (211,   0,   0, 255);
    BLUE      : constant Color := (  0,  64, 195, 255);
    WHITE     : constant Color := (255, 255, 255, 255);
    BLACK     : constant Color := (  0,   0,   0, 255);

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

    procedure SetWindowMinSize(width, height: Integer) with
        Import => True,
        Convention => C,
        External_Name => "SetWindowMinSize";

    function IsWindowResized return C_Bool with
        Import => True,
        Convention => C,
        External_Name => "IsWindowResized";

    function GetScreenWidth return Integer with
        Import => True,
        Convention => C,
        External_Name => "GetScreenWidth";

    function GetScreenHeight return Integer with
        Import => True,
        Convention => C,
        External_Name => "GetScreenHeight";

    procedure SetConfigFlags(flags: unsigned) with
        Import => True,
        Convention => C,
        External_Name => "SetConfigFlags";

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

    procedure BeginTextureMode(texture: RenderTexture) with
        Import => True,
        Convention => C,
        External_Name => "BeginTextureMode";
        
    procedure EndTextureMode with
        Import => True,
        Convention => C,
        External_Name => "EndTextureMode";

    procedure SetTargetFPS(fps: int) with
        Import => True,
        Convention => C,
        External_Name => "SetTargetFPS";

    function GetFontDefault return Font with
        Import => True,
        Convention => C,
        External_Name => "GetFontDefault";

    function LoadFont(fileName: char_array) return Font with
        Import => True,
        Convention => C,
        External_Name => "LoadFont";

    function LoadFontEx(fileName: char_array; fontSize: int; codepoints: access int; codepointCount: int) return Font with
        Import => True,
        Convention => C,
        External_Name => "LoadFontEx";

    procedure UnloadFont(fnt: Font) with
        Import => True,
        Convention => C,
        External_Name => "UnloadFont";

    function LoadRenderTexture(width, height: Integer) return RenderTexture with
        Import => True,
        Convention => C,
        External_Name => "LoadRenderTexture";

    procedure UnloadRenderTexture(texture: RenderTexture) with
        Import => True,
        Convention => C,
        External_Name => "UnloadRenderTexture";

    procedure DrawTextureRec(texture: Texture2D; rec: Rectangle; pos: Vector2; tint: Color) with
        Import => True,
        Convention => C,
        External_Name => "DrawTextureRec";

    procedure DrawText(text: in char_array; posX, posY, fontSize: Integer; col: Color) with
        Import => True,
        Convention => C,
        External_Name => "DrawText";

    procedure DrawTextEx(fnt: Font; text: char_array; pos: Vector2; fontSize, spacing: Float; col: Color) with
        Import => True,
        Convention => C,
        External_Name => "DrawTextEx";

    function MeasureTextEx(fnt: Font; text: char_array; fontSize, spacing: Float) return Vector2 with
        Import => True,
        Convention => C,
        External_Name => "MeasureTextEx";

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

    procedure DrawRectangleRec(rec: Rectangle; col: Color) with
        Import => True,
        Convention => C,
        External_Name => "DrawRectangleRec";

    procedure DrawRectangleLines(posX, posY, width, height: Integer; col: Color) with
        Import => True,
        Convention => C,
        External_Name => "DrawRectangleLines";

    procedure DrawRectangleLinesEx(rec: Rectangle; thick: Float; col: Color) with
        Import => True,
        Convention => C,
        External_Name => "DrawRectangleLinesEx";

    procedure SetTextureFilter(texture: Texture2D; filter: TextureFilter) with
        Import => True,
        Convention => C,
        External_Name => "SetTextureFilter";

    function CheckCollisionPointRec(point: Vector2; rec: Rectangle) return C_Bool with
        Import => True,
        Convention => C,
        External_Name => "CheckCollisionPointRec";

    function IsMouseButtonPressed(btn: MouseButton) return C_Bool with
        Import => True,
        Convention => C,
        External_Name => "IsMouseButtonPressed";

    function GetMousePosition return Vector2 with
        Import => True,
        Convention => C,
        External_Name => "GetMousePosition";

    function IsKeyPressed(key: KeyboardKey) return C_Bool with
        Import => True,
        Convention => C,
        External_Name => "IsKeyPressed";

    procedure DrawCenteredText(text: String; size: Float; fnt: Font; col: Color; rec: Rectangle);
    
end Raylib;
