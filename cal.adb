with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting; 

with Interfaces.C; use Interfaces.C; 
with Raylib; use Raylib;

package body Cal is

-- Private Helper --------------------------------------------------------------

    function is_leap_year(year: Year_Number) return Boolean is
    begin
        return ((year mod 4)   = 0 and 
                (year mod 100) = 0 and
                (year mod 400) = 0
        );
    end;

    function days_in_month(year: Year_Number; month: Month_Number) return Integer is
    begin
        return (
            case month is
                when 1  => 31,
                when 2  => (if is_leap_year(year) then 29 else 28),
                when 3  => 31,
                when 4  => 30,
                when 5  => 31,
                when 6  => 30,
                when 7  => 31,
                when 8  => 31,
                when 9  => 30,
                when 10 => 31,
                when 11 => 30,
                when 12 => 31
        );
    end;

    function week_num_of(year : Year_Number; month : Month_Number; 
                         day  : Day_Number) return Week_Number 
    is
        y : Integer := Integer(year);
        m : Integer := Integer(month);
        d : Integer := Integer(day);
        c : Integer;
    begin
        if m < 3 then
            y := y - 1;
            m := m + 10;
        else
            m := m - 2;
        end if;

        c := y / 100;
        y := y mod 100;
        return Week_Number(((26 * m - 2)/10 + d + y + y/4 + c/4 - 2*c) mod 7);
    end;

    function day_name_of(num: Week_Number) return Day_Name is
    begin
        return (
            case num is
                when 0 => Sunday,
                when 1 => Monday,
                when 2 => Tuesday,
                when 3 => Wednesday,
                when 4 => Thursday,
                when 5 => Friday,
                when 6 => Saturday
        );
    end;

    function month_start(ntime : Time) return Integer is
    begin
        return Integer(week_num_of(AC.Year(ntime), AC.Month(ntime), 1));
    end;

-- Private Drawing -------------------------------------------------------------

    procedure DrawCalendarBackground(pos: Vector2; rows: Integer) is
        cell_w : Float := Float(cell_width);
        cell_h : Float := Float(cell_height);
    begin
        DrawRectangleRec((pos.x, pos.y, cell_w, cell_h), RED);
        DrawRectangleRec((pos.x + cell_w, pos.y, cell_w * 5.0, cell_h), GRAY);
        DrawRectangleRec((pos.x + cell_w * 6.0, pos.y, cell_w, cell_h), BLUE);

        DrawRectangleRec((pos.x, pos.y + cell_h, cell_w, cell_h * Float(rows)), PINK);
        DrawRectangleRec((pos.x + cell_w * 6.0, pos.y + cell_h, cell_w, cell_h * Float(rows)), LIGHTBLUE);
    end;

    procedure DrawCalendarHeader(pos: Vector2; fnt: Font) is
        day_text      : String (1..2);
        cell_x        : Integer;
        cell          : Rectangle;
    begin
        for i in Week_Number'Range loop
            day_text      := day_name_of(i)'Image(1..2);
            cell_x        := Integer(pos.x) + Integer(i) * cell_width;
            cell := (Float(cell_x), pos.y, Float(cell_width), Float(cell_height));

            DrawCenteredText(day_text, fnt_size, fnt, WHITE, cell);
        end loop;
    end;

    procedure DrawCalendarNumbers(pos: Vector2; fnt: Font; year, month, start_day: Integer) is
        cell_x : Float;
        cell_y : Float;
        cell   : Rectangle;
    begin
        for i in start_day..(days_in_month(year, month) + start_day - 1) loop

            cell_x := pos.x + Float(cell_width * (i mod 7));
            cell_y := pos.y + Float(cell_height * (i / 7)) + Float(cell_height);
            cell   := (cell_x, cell_y, Float(cell_width), Float(cell_height));

            DrawCenteredText(Integer'Image(i - start_day + 1), fnt_size, fnt, BLACK, cell);
        end loop;
    end;

    procedure DrawCalendarGrid(pos: Vector2; rows: Integer) is
        cell_w : Float := Float(cell_width);
        cell_h : Float := Float(cell_height);
        calendar_w : Float := cell_w * 7.0;
        calendar_h : Float := cell_h * (Float(rows) + 1.0);
    begin
        for i in 2..rows loop
            DrawLineEx((pos.x, pos.y + cell_h * Float(i)), (pos.x + calendar_w, pos.y + cell_h * Float(i)), 2.0, LIGHTGREY); 
        end loop;

        DrawLineEx((pos.x + cell_w, pos.y + cell_h), (pos.x + cell_w, pos.y + calendar_h), 2.0, LIGHTGREY);
        DrawLineEx((pos.x + cell_w * 2.0, pos.y + cell_h), (pos.x + cell_w * 2.0, pos.y + calendar_h), 2.0, LIGHTGREY);
        DrawLineEx((pos.x + cell_w * 3.0, pos.y + cell_h), (pos.x + cell_w * 3.0, pos.y + calendar_h), 2.0, LIGHTGREY);
        DrawLineEx((pos.x + cell_w * 4.0, pos.y + cell_h), (pos.x + cell_w * 4.0, pos.y + calendar_h), 2.0, LIGHTGREY);
        DrawLineEx((pos.x + cell_w * 5.0, pos.y + cell_h), (pos.x + cell_w * 5.0, pos.y + calendar_h), 2.0, LIGHTGREY);
        DrawLineEx((pos.x + cell_w * 6.0, pos.y + cell_h), (pos.x + cell_w * 6.0, pos.y + calendar_h), 2.0, LIGHTGREY);

        DrawLineEx((pos.x, pos.y + cell_h), (pos.x + calendar_w, pos.y + cell_h), 2.0, BLACK);
        DrawLineEx((pos.x + cell_w, pos.y), (pos.x + cell_w, pos.y + cell_h), 2.0, BLACK);
        DrawLineEx((pos.x + cell_w * 2.0, pos.y), (pos.x + cell_w * 2.0, pos.y + cell_h), 2.0, BLACK);
        DrawLineEx((pos.x + cell_w * 3.0, pos.y), (pos.x + cell_w * 3.0, pos.y + cell_h), 2.0, BLACK);
        DrawLineEx((pos.x + cell_w * 4.0, pos.y), (pos.x + cell_w * 4.0, pos.y + cell_h), 2.0, BLACK);
        DrawLineEx((pos.x + cell_w * 5.0, pos.y), (pos.x + cell_w * 5.0, pos.y + cell_h), 2.0, BLACK);
        DrawLineEx((pos.x + cell_w * 6.0, pos.y), (pos.x + cell_w * 6.0, pos.y + cell_h), 2.0, BLACK);

        DrawRectangleLinesEx((pos.x, pos.y, Float(calendar_w), Float(calendar_h)), 2.0, BLACK);
    end;

    procedure DrawDayHighlight(pos: Vector2; day, start_day: Integer) is
        cell_x : Float;
        cell_y : Float;
    begin
        cell_x := pos.x + Float(cell_width * ((day + start_day - 1) mod 7));
        cell_y := pos.y + Float(cell_height * ((day + start_day) / 7)) + Float(cell_height);
        DrawRectangleRec((cell_x, cell_y, Float(cell_width), Float(cell_height)), (0,255,0, 150));
    end;

-- Public ----------------------------------------------------------------------

    function CalendarMake(pos: Vector2) return Calendar_T is
        cal_fnt : Font := LoadFont("Retron2000.ttf");
    begin 
        SetTextureFilter(cal_fnt.texture, TEXTURE_FILTER_BILINEAR);
        return (
            pos => pos,
            fnt => cal_fnt,
            tex => LoadRenderTexture(231, 200),
            others => <>
        );
    end;

    procedure CalendarDestory(cal: Calendar_T) is
    begin
        UnloadRenderTexture(cal.tex);
        UnloadFont(cal.fnt);
    end;
    
    procedure Draw(self: in out Calendar_T) is
        year  : Year_Number;
        month : Month_Number;
        day   : Day_Number;  

        cells_needed : Integer;
        rows_needed  : Integer;
    begin
        if self.dirty then
            year  := AC.Year(self.now);
            month := AC.Month(self.now);
            day   := AC.Day(self.now);

            cells_needed := self.start_day + days_in_month(year, month);
            rows_needed  := Integer(Float'Ceiling(Float(cells_needed) / 7.0));

            BeginTextureMode(self.tex);
                ClearBackground(WHITE);

                -- Drawing occurs realitive to the top left of texture
                DrawCalendarBackground((0.0, 0.0), rows_needed);
                DrawCalendarHeader((0.0, 0.0), self.fnt);
                DrawDayHighlight((0.0, 0.0), day, self.start_day);
                DrawCalendarNumbers((0.0, 0.0), self.fnt, year, month, self.start_day);
                DrawCalendarGrid((0.0, 0.0), rows_needed);
            EndTextureMode;

            self.dirty := False;
        end if;

        DrawTextureRec(self.tex.texture, 
                       (0.0, 0.0, Float(self.tex.texture.width), Float(-self.tex.texture.height)), 
                       self.pos, 
                       WHITE);
    end;

    function CalendarCellClicked(self: Calendar_T) return Integer is
        cal_w      : Float     := Float(cell_width * 7);
        cal_h      : Float     := Float(cell_height * 6);
        cal_bounds : Rectangle := (self.pos.x, self.pos.y + Float(cell_height), cal_w, cal_h);
        mousePos   : Vector2   := GetMousePosition;

        cell_x : Integer;
        cell_y : Integer;
    begin
        if CheckCollisionPointRec(mousePos, cal_bounds) and
           IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then

            cell_y := Integer(mousePos.y - (self.pos.y + Float(cell_height))) / cell_height;
            cell_x := Integer(mousePos.x - self.pos.x) / cell_width;

            return cell_y * 7 + cell_x - self.start_day + 1;
        end if;

        return 0;
    end;

end Cal;
