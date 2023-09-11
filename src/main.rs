use std::ops::Deref;
use std::ops::Add;
use std::thread::sleep;
use std::time::Duration;
use minifb::{Window, WindowOptions};
use rand::Rng;
use raqote::{DrawTarget, SolidSource, Source, DrawOptions, PathBuilder};

#[derive(Debug)]
struct CourseLineSeg (CoursePoint, CoursePoint);

#[derive(Copy, Clone, Debug)]
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

impl CourseLineSeg {
    fn intersects(&self, other: &Self) -> bool {
        // Self::intersect(self.0, other.0, self.1, other.1)

		use line_intersection::LineInterval;
		use geo::Line;

		let segment = LineInterval::line_segment(Line {
			start: (self.0.downcourse as f64, self.0.sidecourse as f64).into(),
			end: (self.1.downcourse as f64, self.1.sidecourse as f64).into(),
		});

		let line = LineInterval::line(Line {
			start: (other.0.downcourse as f64, other.0.sidecourse as f64).into(),
			end: (other.1.downcourse as f64, other.1.sidecourse as f64).into(),
		});

		let intersection = segment.relate(&line).unique_intersection();
		intersection.is_some()
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

struct SkierPath {
    prev: Option<Box<SkierPath>>,
    current: CoursePoint
}

impl SkierPath {
	fn pop(&mut self) {
		let temp = std::mem::take(&mut self.prev);
		if let Some(mut t) = temp {
			self.prev = std::mem::take(&mut t.prev);
		}
		else {
			unreachable!();
		}
	}
}

impl Default for SkierPath {
    fn default() -> Self {
        Self { prev: Default::default(), current: CoursePoint { downcourse: 0, sidecourse: 0 } }
    }
}

impl Drop for SkierPath {
    fn drop(&mut self) {
		let mut pop_count = 0;
        while self.prev.is_some() {
			self.pop();
			pop_count += 1;
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
        let current = self.ski_path.current + CoursePoint::create(self.downcourse_speed, self.sidecourse_speed);
		let previous = std::mem::take(&mut self.ski_path);
		self.ski_path = SkierPath {
            prev: Some(Box::new(previous)),
            current
        };
        self.ski_path.prev.as_ref().unwrap().current
    }
}

#[derive(Clone)]
struct SkierAgent {
	call_count: usize,
    moves: [(i8, i8); 4000],
}

impl SkierAgent {
	fn revise_prev_accel(&mut self) {
		self.call_count -= 1;
		self.mutate_index(self.call_count);
	}
    fn get_next_acceleration(&mut self) -> (i32, i32) {
        let (left, right) = self.moves[self.call_count];
		self.call_count += 1;
		(left as i32, right as i32)
    }
	fn mutate_index(&mut self, index: usize) {
		let mut rng = rand::thread_rng();
		self.moves[index] = (rng.gen::<i8>() % 10, rng.gen::<i8>() % 10);
	}
	fn mutate(&mut self)  {
		self.mutate_index(rand::thread_rng().gen::<usize>() % 4000)
	}
}

impl Default for SkierAgent {
    fn default() -> Self {
        Self { call_count:0, moves: [(0,0); 4000] }
    }
}

struct ViewScreen {
    size: (usize, usize),
    dt: DrawTarget,
    window: Window,
}

#[derive(Debug)]
struct SkiScore {
	entry_gates: bool,
	exit_gates: bool,
	score: u16
}

impl SkiScore {
	fn missed_entry() -> Self {
		SkiScore { entry_gates: false, exit_gates: false, score: 0 }
	}
	fn no_exit( score: u16) -> Self {
		SkiScore { entry_gates: true, exit_gates: false, score }
	}
	fn full_pass() -> Self {
		SkiScore { entry_gates: false, exit_gates: false, score: 600 }
	}
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
        let mut floated = ((loc.sidecourse as f32) / 20.0, (loc.downcourse as f32) / 20.0);
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
	fn draw_segment(&mut self, segment: &CourseLineSeg) {
		let mut pb = PathBuilder::new();
		let (px, py) = Self::convert_course_point(segment.0);
		let (qx, qy) = Self::convert_course_point(segment.1);
		let length = qx - px;
		println!("{length}, {px} {qx}");
		pb.rect(px, py, length, 1.0);
		let path = pb.finish();
		self.dt.fill(&path, &Source::Solid(SolidSource::from_unpremultiplied_argb(0xff, 0, 0xff, 0)), &DrawOptions::new());
	}
	fn draw_skier(&mut self, skier_location: &SkierPath) {
        let mut next: &SkierPath = skier_location;
        loop {
            let mut pb = PathBuilder::new();
            let (side, down) = Self::convert_course_point(next.current);
            pb.arc(side, down, 0.75, 0., 2. * 3.14159);
            let path = pb.finish();
            self.dt.fill(&path, &Source::Solid(SolidSource::from_unpremultiplied_argb(0xff, 0, 0, 0xff)), &DrawOptions::new());
            match &next.prev {
                None => return,
                Some(x) => {
					next = x.deref();
				}
            }
        }
    }
    fn window_loop(&mut self) {
        if self.is_open() {
            self.window.update_with_buffer(self.dt.get_data(), self.size.0, self.size.1).unwrap();
            self.dt.clear(SolidSource::from_unpremultiplied_argb(0xff, 0xEE, 0xEE, 0xff));
        }
    }
}

fn score_skier_agent(mut skier_agent: SkierAgent, course: &mut SlalomCourse, viewscreen: &mut ViewScreen) -> (SkiScore, Skier) {
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
		let mut previous;
		loop {
			let accel = skier_agent.get_next_acceleration();
			previous = skier.next_tic(accel);
			let distance: f64 = skier.ski_path.current.distance_from(&CoursePoint::create(boat_location, 0));
			let fudge: f64 = distance - f64::from(rope_length);
			if fudge.abs() > skier_reach {
				skier_agent.revise_prev_accel();
			}
			else {
				break;
			}
		}
        let segment = CourseLineSeg(previous, skier.ski_path.current);
        if !entry_gates {
			if boat_location > 2700 {
				break;
			}
			if(segment.0.downcourse < 0 && segment.1.downcourse > 0) {
				println!("HAH, {segment:?} {:?}", course.entry_gate);
			}
            if course.entry_gate.intersects(&segment) {
                entry_gates = true;
				println!("got_there");
            }
			if(segment.0.downcourse < 0 && segment.1.downcourse > 0) {
				println!("Oh!");
				break;
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
			if (true, true, true) != buoys[x] && boat_location > ((41 * (x as i32 + 2) - 14) * 100) {
				break;
			}
        }
        x += 1;
        x %= 500;
        if x == 0 {
            course.draw_buoys(viewscreen);
            viewscreen.draw_boat(boat_location);
            viewscreen.draw_skier(&skier.ski_path);
            viewscreen.window_loop();
        }
    }
    if !entry_gates {
        return (SkiScore::missed_entry(), skier);
    }
    for x in 0..5u16 {
        match buoys[x as usize] {
            (true, true, true) => continue,
            (true, true, false) => return (SkiScore::no_exit(x * 100 + 50), skier),
            (true, false, _) => return (SkiScore::no_exit(x * 100 + 25), skier),
            (false, _, _) => return (SkiScore::no_exit(x), skier),
        }
    }
    if exit_gates {
        return (SkiScore::full_pass(), skier);
    }
    else {
        return (SkiScore::no_exit(600), skier);
    }
}


fn tests() {
	let gates = CourseLineSeg(CoursePoint { downcourse: 0, sidecourse: 125 }, CoursePoint { downcourse: 0, sidecourse: -125 });
	let skier = CourseLineSeg(CoursePoint { downcourse: -5, sidecourse: 0 }, CoursePoint { downcourse: 5, sidecourse: 0 });
	assert!(gates.intersects(&skier));
}


fn main() {
	tests();
    let mut viewscreen = ViewScreen::create();
    let mut course = SlalomCourse::create();
    let (score, skier) = score_skier_agent(SkierAgent::default(), &mut course, &mut viewscreen);
    println!("score: {score:?}");
	course.draw_buoys(&mut viewscreen);
    viewscreen.draw_skier(&skier.ski_path);
	while viewscreen.is_open() {
		sleep(Duration::from_millis(200));
		viewscreen.window_loop();

    }
}