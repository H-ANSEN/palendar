with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting; 

with Interfaces.C; use Interfaces.C; 
with Raylib; use Raylib;

package body Cal is

-- Private Forward Decl --------------------------------------------------------

function is_leap_year(year: Year_Number) return Boolean;
function week_num_of(year, month, day: Integer) return Week_Number;
function day_name_of(num: Week_Number) return Day_Name;

procedure DrawCalendarBackground(pos: Vector2; rows: Integer);
procedure DrawCalendarHeader(pos: Vector2; fnt: Font);
procedure DrawCalendarNumbers(pos: Vector2; fnt: Font; year, month, start_day: Integer);
procedure DrawCalendarGrid(pos: Vector2; rows: Integer);
procedure DrawDayHighlight(pos: Vector2; cal: Calendar_T; day: Integer; col: Color);
procedure DrawYearMonth(pos: Vector2; year, month: Integer; fnt: Font);

-- Public ----------------------------------------------------------------------

    overriding procedure Draw(self: in out Calendar_T) is
        year  : Year_Number  := AC.Year(self.now);
        month : Month_Number := AC.Month(self.now);
        day   : Day_Number   := AC.Day(self.now);

        pos   : Vector2      := (self.x, self.y);

        cells_needed : Integer;
        rows_needed  : Integer;
    begin
        cell_width   := Integer(self.width / 7.0);
        cell_height  := Integer(self.width / 6.0);

        cells_needed := self.start_day + days_in_month(year, month);
        rows_needed  := Integer(Float'Ceiling(Float(cells_needed) / 7.0));

        DrawCalendarBackground(pos, rows_needed);
        DrawCalendarHeader(pos, self.fnt);
        DrawCalendarNumbers(pos, self.fnt, year, month, self.start_day);
        DrawDayHighlight(pos, self, day, (0, 255, 0, 150));
        DrawCalendarGrid(pos, rows_needed);
        DrawYearMonth(pos, year, month, self.fnt);

        if self.CalendarCellHovered /= -1 then
            DrawDayHighlight(pos, self, self.CalendarCellHovered, (0, 0, 38, 140));
        end if;
    end;

    overriding procedure Update(self: in out Calendar_T) is
    begin
        null;
    end;

    function CalendarCellHovered(self: Calendar_T) return Integer is
        cal_w      : Float     := Float(cell_width * 7);
        cal_h      : Float     := Float(cell_height * 6);
        cal_bounds : Rectangle := (self.x, self.y + Float(cell_height), cal_w, cal_h);
        mousePos   : Vector2   := GetMousePosition;

        index  : Integer;
        cell_x : Integer;
        cell_y : Integer;
    begin
        if CheckCollisionPointRec(mousePos, cal_bounds) then
            cell_y := Integer(mousePos.y - (self.y + Float(cell_height))) / cell_height;
            cell_x := Integer(mousePos.x - self.x) / cell_width;
            index  := cell_y * 7 + cell_x - self.start_day + 1;

            if index > 0 and index <= days_in_month(AC.Year(self.now), AC.Month(self.now)) then
                return index;
            else 
                return -1;
            end if;
        end if;

        return -1;
    end;

    procedure CalendarIncrement(self: in out Calendar_T) is
        nyear  : Year_Number  := AC.Year(self.now);
        nmonth : Month_Number := AC.Month(self.now);
        nday   : Day_Number   := AC.Day(self.now);
    begin
        nmonth := (if nmonth = 12 then 1 else nmonth + 1);
        nyear  := (if nmonth = 1 then nyear + 1 else nyear);

        self.now        := AC.Time_Of(nyear, nmonth, nday);
        self.start_day  := month_start(self.now);
        self.month_days := days_in_month(AC.Year(self.now), AC.Month(self.now));
    end;

-- Private Helper --------------------------------------------------------------

    function is_leap_year(year: Year_Number) return Boolean is
    begin
        return ((year mod 4)   = 0 and 
                (year mod 100) = 0 and
                (year mod 400) = 0);
    end;

    function days_in_month(year: Year_Number; month: Month_Number) return Integer is
    begin
        return (case month is
                when 1  => 31, when 2  => (if is_leap_year(year) then 29 else 28),
                when 3  => 31, when 4  => 30, when 5  => 31, when 6  => 30,
                when 7  => 31, when 8  => 31, when 9  => 30, when 10 => 31,
                when 11 => 30, when 12 => 31);
    end;

    function week_num_of(year, month, day: Integer) return Week_Number 
    is
        y : Integer := year;
        m : Integer := month;
        d : Integer := day;
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
        return (case num is
                when 0 => Sunday, when 1 => Monday, when 2 => Tuesday, 
                when 3 => Wednesday, when 4 => Thursday, when 5 => Friday,
                when 6 => Saturday);
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
            day_text := day_name_of(i)'Image(1..2);
            cell_x   := Integer(pos.x) + Integer(i) * cell_width;
            cell     := (Float(cell_x), pos.y, Float(cell_width), Float(cell_height));

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

    procedure DrawDayHighlight(pos: Vector2; cal: Calendar_T; day: Integer; col: Color) is
        cell_x : Float;
        cell_y : Float;
    begin
        cell_x := pos.x + Float(cell_width * ((day + cal.start_day - 1) mod 7));
        cell_y := pos.y + Float(cell_height * ((day + cal.start_day - 1) / 7)) + Float(cell_height);
        DrawRectangleRec((cell_x + 1.0, cell_y + 1.0, Float(cell_width - 1), Float(cell_height - 1)), col);
    end;

    procedure DrawYearMonth(pos: Vector2; year, month: Integer; fnt: Font) is
        rec : Rectangle := (pos.x, pos.y - Float(cell_height) + 1.0, Float(cell_width) * 7.0, Float(cell_height));
        str : String := Integer'Image(month) & "/" & Integer'Image(year);
    begin
        --DrawRectangleLinesEx(rec, 2.0, BLACK);
        DrawCenteredText(Integer'Image(month) & "/" & Integer'Image(year), fnt_size, fnt, BLACK, rec);
    end;

end Cal;
