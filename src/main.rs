use std::mem::swap;
use std::rc::Rc;
use std::{ops::Add};
use minifb::{MouseMode, Window, WindowOptions, ScaleMode, Scale};
use raqote::{DrawTarget, SolidSource, Source, DrawOptions, PathBuilder, Point, Transform, StrokeStyle, Path};
use font_kit::family_name::FamilyName;
use font_kit::properties::Properties;
use font_kit::source::SystemSource;

struct CourseLineSeg (CoursePoint, CoursePoint);

#[derive(Copy, Clone)]
struct CoursePoint {
    downcourse: i32,
    sidecourse: i32
}

struct BuoySegs {
    loc: CoursePoint,
    quarter_mark: CourseLineSeg,
    half_mark: CourseLineSeg,
    full_mark: CourseLineSeg,
}
struct SlalomCourse {
    entry_gate: CourseLineSeg,
    buoys: [BuoySegs; 6],
    exit_gate: CourseLineSeg
}

impl BuoySegs {
    fn create(buoy_location: CoursePoint) -> Self {
        let quarter_mark: CourseLineSeg;
        let half_mark: CourseLineSeg = CourseLineSeg(buoy_location, buoy_location + CoursePoint::create(4100, 0));
        let buoy_downcourse = buoy_location.downcourse;
        let full_mark: CourseLineSeg; 
        if buoy_location.sidecourse > 0 {
            quarter_mark = CourseLineSeg(buoy_location, buoy_location + CoursePoint::create(0, 1000));
            full_mark = CourseLineSeg(CoursePoint::create(buoy_downcourse, 115), CoursePoint::create(buoy_downcourse + 4100, 115));
        }
        else {
            quarter_mark = CourseLineSeg(buoy_location, buoy_location + CoursePoint::create(0, -1000));
            full_mark = CourseLineSeg(CoursePoint::create(buoy_downcourse, -115), CoursePoint::create(buoy_downcourse + 4100, -115));
        }
        Self {
            loc: buoy_location,
            quarter_mark,
            half_mark,
            full_mark
        }
    }
}

#[derive(PartialEq)]
enum SegOrientation {
    Colinear,
    Clockwise,
    CouterClockwise,
}

impl CourseLineSeg {
    fn on_segment( p: CoursePoint, q:  CoursePoint, r: CoursePoint) -> bool {
        q.downcourse <= std::cmp::max(p.downcourse, r.downcourse) && 
        q.downcourse >= std::cmp::min(p.downcourse, r.downcourse) &&
        q.sidecourse <= std::cmp::max(p.sidecourse, r.sidecourse) &&
        q.sidecourse >= std::cmp::min(p.sidecourse, r.sidecourse)
    }
    fn orientation(p: CoursePoint, q: CoursePoint, r: CoursePoint) -> SegOrientation{
        match (q.sidecourse - p.sidecourse) * (r.downcourse - q.downcourse) - (q.downcourse - p.downcourse) * (r.sidecourse - q.sidecourse)
        {
            0 => SegOrientation::Colinear,
            1..=i32::MAX => SegOrientation::Clockwise,
            i32::MIN..=-1 => SegOrientation::CouterClockwise,
        }
    }

    fn intersect(p1: CoursePoint, q1: CoursePoint, p2: CoursePoint, q2: CoursePoint) -> bool
    {
        // Find the four orientations needed for general and
        // special cases
        let o1: SegOrientation = Self::orientation(p1, q1, p2);
        let o2: SegOrientation = Self::orientation(p1, q1, q2);
        let o3: SegOrientation = Self::orientation(p2, q2, p1);
        let o4: SegOrientation = Self::orientation(p2, q2, q1);
    
        // General case
        if o1 != o2 && o3 != o4 {
            return true;
        }
            
    
        // Special Cases
        // p1, q1 and p2 are collinear and p2 lies on segment p1q1
        if o1 == SegOrientation::Colinear && Self::on_segment(p1, p2, q1) {
            return true;
        } 
    
        // p1, q1 and q2 are collinear and q2 lies on segment p1q1
        if o2 == SegOrientation::Colinear  && Self::on_segment(p1, q2, q1) {
            return true;
        }
    
        // p2, q2 and p1 are collinear and p1 lies on segment p2q2
        if o3 == SegOrientation::Colinear && Self::on_segment(p2, p1, q2) {
            return true;
        }
    
        // p2, q2 and q1 are collinear and q1 lies on segment p2q2
        if o4 == SegOrientation::Colinear && Self::on_segment(p2, q1, q2) {
            return true;
        }

        return false 
    }


    fn intersects(&self, other: &Self) -> bool {
        Self::intersect(self.0, other.0, self.1, other.1)
    }
}
impl Add for CoursePoint {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        CoursePoint::create(self.downcourse + rhs.downcourse, self.sidecourse + rhs.sidecourse)
    }
}
impl CoursePoint {
    fn distance_from(&self, other: &Self) -> f64 {
        f64::sqrt(((self.downcourse - other.downcourse) * (self.downcourse - other.downcourse) +
        (self.sidecourse - other.sidecourse) * (self.sidecourse - other.sidecourse)).into())
    }
    fn create(downcourse: i32, sidecourse: i32) -> Self {
        Self {
            downcourse,
            sidecourse
        }
    }
}

impl SlalomCourse {
    const END_GATE_DOWNCOURSE: i32 = (41 * 5 + 27 * 2) * 100;
    fn create() -> SlalomCourse {
        Self {
            entry_gate: CourseLineSeg(CoursePoint::create(0, 125),CoursePoint::create(0, -125)),
            exit_gate: CourseLineSeg(CoursePoint::create(Self::END_GATE_DOWNCOURSE, 125),CoursePoint::create(Self::END_GATE_DOWNCOURSE, -125)),
            buoys: [
                BuoySegs::create(CoursePoint::create(2700, 1150)),
                BuoySegs::create(CoursePoint::create(2700 + 4100, -1150)),
                BuoySegs::create(CoursePoint::create(2700 + 2 * 4100, 1150)),
                BuoySegs::create(CoursePoint::create(2700 + 3 * 4100, -1150)),
                BuoySegs::create(CoursePoint::create(2700 + 4 * 4100, 1150)),
                BuoySegs::create(CoursePoint::create(2700 + 5 * 4100, -1150)),
            ],
        }
    }

    fn draw_buoys(&self, viewscreen: &mut ViewScreen) {
        for x in self.buoys.iter() {
            viewscreen.draw_buoy(x.loc);
        }
        viewscreen.draw_buoy(CoursePoint::create(0,125));
        viewscreen.draw_buoy(CoursePoint::create(0,-125));
        viewscreen.draw_buoy(CoursePoint::create(Self::END_GATE_DOWNCOURSE,125));
        viewscreen.draw_buoy(CoursePoint::create(Self::END_GATE_DOWNCOURSE,-125));

    }
}

#[derive(Clone)]
struct SkierPath {
    prev: Option<Rc<SkierPath>>,
    current: CoursePoint
}

impl Drop for SkierPath {
    fn drop(&mut self) {
        let mut swapped: Option<Rc<SkierPath>>;
        while self.prev.is_some() {
            swapped = None;
            std::mem::swap(&mut temp, &mut swapped.as_ref().unwrap().prev);
        }
    }
}

struct Skier {
    downcourse_speed: i32,
    sidecourse_speed: i32,
    ski_path: SkierPath,
    max_accel: f64,
}

impl Skier {
    fn create(start_loc: CoursePoint) -> Self {
        Self {downcourse_speed: 10, sidecourse_speed: 0, ski_path: SkierPath { prev: None, current: start_loc }, max_accel: 0.0}
    }
    fn next_tic(&mut self, acceleration: (i32, i32)) -> CoursePoint {
        let (down_a, side_a) = acceleration;
        let absolute_acceleration = f64::sqrt((down_a * down_a + side_a * side_a).into());
        if self.max_accel < absolute_acceleration {
            self.max_accel = absolute_acceleration;
        }
        self.downcourse_speed += down_a;
        self.sidecourse_speed += side_a;
        let previous = self.ski_path.clone();
        let current = previous.current + CoursePoint::create(self.downcourse_speed, self.sidecourse_speed); 
        self.ski_path = SkierPath {
            prev: Some(Rc::new(previous)),
            current
        };
        self.ski_path.prev.as_ref().unwrap().current
    }
}

struct SkierAgent {
    call_count: u32
}

impl SkierAgent {
    fn get_next_acceleration(&mut self) -> (i32, i32) {
        self.call_count += 1;
        println!("{}", self.call_count);
        (0,0)
    }
    fn create() -> Self {
        Self { call_count:0 }
    }
}

struct ViewScreen {
    size: (usize, usize),
    dt: DrawTarget,
    window: Window,
}

impl ViewScreen {

    fn create()-> Self {
        const WIDTH: usize = 800;
        const HEIGHT: usize = 800;
        let window = Window::new("Raqote", WIDTH, HEIGHT, WindowOptions {
            ..WindowOptions::default()
        }).unwrap();
        let size = window.get_size();
        Self {
            window,
            size,
            dt: DrawTarget::new(size.0 as i32, size.1 as i32)
        }
    }

    fn is_open(&self) -> bool {
        self.window.is_open()
    }
    fn convert_course_point(loc:CoursePoint) -> (f32, f32) {
        let mut floated = ((loc.sidecourse as f32) / 40.0, (loc.downcourse as f32) / 40.0);
        floated.0 += 400.0;
        floated.1 = 700.0 - floated.1;
        floated
    }
    fn draw_buoy(&mut self, loc: CoursePoint) {
        let mut pb = PathBuilder::new();
        let (side, down) = Self::convert_course_point(loc);
        pb.arc(side, down, 2.0, 0., 2. * 3.14159);
        let path = pb.finish();
        self.dt.fill(&path, &Source::Solid(SolidSource::from_unpremultiplied_argb(0xff, 0xff, 0, 0)), &DrawOptions::new());
    }
    fn draw_boat(&mut self, boat_loc: i32) {
        let mut pb = PathBuilder::new();
        let (side, down) = Self::convert_course_point(CoursePoint::create(boat_loc, 0));
        pb.arc(side, down, 4.0, 0., 2. * 3.14159);
        let path = pb.finish();
        self.dt.fill(&path, &Source::Solid(SolidSource::from_unpremultiplied_argb(0xff, 0, 0xff, 0)), &DrawOptions::new());
    }
    fn draw_skier(&mut self, skier_location: Rc<SkierPath>) {
        let mut next = skier_location;
        loop {
            let mut pb = PathBuilder::new();
            let (side, down) = Self::convert_course_point(next.current);
            pb.arc(side, down, 0.75, 0., 2. * 3.14159);
            let path = pb.finish();
            self.dt.fill(&path, &Source::Solid(SolidSource::from_unpremultiplied_argb(0xff, 0, 0, 0xff)), &DrawOptions::new());
            match &next.prev {
                None => return,
                Some(x) => next = Rc::clone(x),
            }
        }
    }
    fn window_loop(&mut self) {
        if self.is_open() {
            self.window.update_with_buffer(self.dt.get_data(), self.size.0, self.size.1).unwrap();
            self.dt.clear(SolidSource::from_unpremultiplied_argb(0xff, 0xff, 0xff, 0xff));
        }
    }
}

fn score_skier_agent(mut skier_agent: SkierAgent, course: &mut SlalomCourse, viewscreen: &mut ViewScreen) -> (u16, Skier) {
    let mut boat_location: i32 = -6000;
    let end_of_course = (41 * 5 + 27 * 2 + 55) * 100;
    // let mut boat_location: i32 = 22250;
    // let end_of_course: i32 = 22300;
    let rope_length = 1825;
    let skier_reach: f64 = 50.0;
    let mut skier = Skier::create(CoursePoint::create(boat_location - rope_length, 0));
    let mut entry_gates = false;
    let mut exit_gates = false;
    let mut buoys: [(bool, bool, bool); 6] = [(false,false,false),(false,false,false),(false,false,false),(false,false,false),(false,false,false),(false,false,false)];
    let mut x = 0;
    while boat_location < end_of_course {
        boat_location += 10;
        let previous = skier.next_tic(skier_agent.get_next_acceleration());
        let distance: f64 = skier.ski_path.current.distance_from(&CoursePoint::create(boat_location, 0));
        let fudge: f64 = distance - f64::from(rope_length);
        if fudge.abs() > skier_reach {
            break;
        }
        let segment = CourseLineSeg(previous, skier.ski_path.current);
        if !entry_gates {
            if course.entry_gate.intersects(&segment) {
                entry_gates = true;
            }
        }
        if !exit_gates {
            if course.exit_gate.intersects(&segment) {
                exit_gates = true;
            }
        }
        for x in 0..6 {
            if !buoys[x].0 {
                if course.buoys[x].quarter_mark.intersects(&segment) {
                    buoys[x].0 = true;
                }
            }
            if !buoys[x].1 {
                if course.buoys[x].half_mark.intersects(&segment) {
                    buoys[x].1 = true;
                }
            }
            if !buoys[x].2 {
                if course.buoys[x].full_mark.intersects(&segment) {
                    buoys[x].2 = true;
                }
            }
        }
        x += 1;
        x %= 500;
        if x == 0 {
            course.draw_buoys(viewscreen);
            viewscreen.draw_boat(boat_location);
            viewscreen.draw_skier(Rc::new(skier.ski_path.clone()));
            viewscreen.window_loop();
        }
    }
    if !entry_gates {
        return (0, skier);
    }
    for x in 0..5u16 {
        match buoys[x as usize] {
            (true, true, true) => continue,
            (true, true, false) => return (x * 100 + 50, skier),
            (true, false, _) => return (x * 100 + 25, skier),
            (false, _, _) => return (x, skier),
        }
    }
    if exit_gates {
        return (600, skier);
    }
    else {
        return (599, skier);
    }
}

fn main() {
    let mut viewscreen = ViewScreen::create();
    let mut course = SlalomCourse::create();
    let (score, skier) = score_skier_agent(SkierAgent::create(), &mut course, &mut viewscreen);
    println!("score: {score}");
    while viewscreen.is_open() {
        course.draw_buoys(&mut viewscreen);
        viewscreen.draw_skier(Rc::new(skier.ski_path.clone()));
        viewscreen.window_loop();
    }
}