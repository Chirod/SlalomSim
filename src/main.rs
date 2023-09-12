use std::{ops::Add, cmp::Ordering};
use std::thread::sleep;
use std::time::Duration;
use minifb::{Window, WindowOptions};
use rand::Rng;
use rand::seq::SliceRandom;
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
	death_line: CourseLineSeg,
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
		let death_line: CourseLineSeg;
        if buoy_location.sidecourse > 0 {
            quarter_mark = CourseLineSeg(buoy_location, buoy_location + CoursePoint::create(0, 1000));
            full_mark = CourseLineSeg(CoursePoint::create(buoy_downcourse, 115), CoursePoint::create(buoy_downcourse + 4100, 115));
			death_line = CourseLineSeg(CoursePoint::create(buoy_downcourse, 115), CoursePoint::create(buoy_downcourse - 5, -8000));
        }
        else {
            quarter_mark = CourseLineSeg(buoy_location, buoy_location + CoursePoint::create(0, -1000));
            full_mark = CourseLineSeg(CoursePoint::create(buoy_downcourse, -115), CoursePoint::create(buoy_downcourse + 4100, -115));
			death_line = CourseLineSeg(CoursePoint::create(buoy_downcourse, -115), CoursePoint::create(buoy_downcourse - 5, 8000));
        }
        Self {
            loc: buoy_location,
            quarter_mark,
            half_mark,
            full_mark,
			death_line
        }
    }
}

impl CourseLineSeg {
    fn intersection(&self, other: &Self) -> Option<geo::Point<f64>> {
        // Self::intersect(self.0, other.0, self.1, other.1)

		use line_intersection::LineInterval;
		use geo::Line;

		let segment = LineInterval::line_segment(Line {
			start: (self.0.downcourse as f64, self.0.sidecourse as f64).into(),
			end: (self.1.downcourse as f64, self.1.sidecourse as f64).into(),
		});

		let line = LineInterval::line_segment(Line {
			start: (other.0.downcourse as f64, other.0.sidecourse as f64).into(),
			end: (other.1.downcourse as f64, other.1.sidecourse as f64).into(),
		});

		let intersection = segment.relate(&line).unique_intersection();
        // let intersection2 = line.relate(&segment).unique_intersection();
        intersection
    }

	fn intersects(&self, other: &Self) -> bool {
		self.intersection(other).is_some()
	}

}

#[test]
fn test_intersects() {
    let a = CourseLineSeg(CoursePoint { downcourse: -7825, sidecourse: 0 }, CoursePoint { downcourse: -7815, sidecourse: 0 });
    let b = CourseLineSeg(CoursePoint { downcourse: 0, sidecourse: 125 }, CoursePoint { downcourse: 0, sidecourse: -125 });
    assert!(!a.intersects(&b));
}


impl Add for CoursePoint {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        CoursePoint::create(self.downcourse.saturating_add(rhs.downcourse), self.sidecourse.saturating_add(rhs.sidecourse))
    }
}
impl CoursePoint {
    fn distance_from(&self, other: &Self) -> f64 {
		let a1 = self.downcourse as f64;
		let b1 = other.downcourse as f64;
		let a2 = self.sidecourse as f64;
		let b2 = other.sidecourse as f64;
		let a = a1 - b1;
		let b = a2 - b2;
        f64::sqrt((a * a) + (b * b))
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
	points: Vec<CoursePoint>,
}

struct Skier {
    downcourse_speed: i32,
    sidecourse_speed: i32,
    ski_path: SkierPath,
    max_accel: f64,
}

impl Skier {
    fn create(start_loc: CoursePoint) -> Self {
        Self {downcourse_speed: 10, sidecourse_speed: 0, ski_path: SkierPath { points: vec![start_loc] }, max_accel: 0.0}
    }
	fn propose_tic(&self, acceleration: (i32, i32)) -> (CoursePoint, CoursePoint) {
		let (down_a, side_a) = acceleration;
        let downcourse_speed = self.downcourse_speed + down_a;
        let sidecourse_speed = self.sidecourse_speed + side_a;
		let previous = *self.ski_path.points.last().unwrap();
		let current = previous + CoursePoint::create(downcourse_speed, sidecourse_speed);
		(previous, current)
	}
    fn commit_tic(&mut self, acceleration: (i32, i32)) {
		let (down_a, side_a) = acceleration;
        let absolute_acceleration = f64::sqrt((down_a * down_a + side_a * side_a).into());
        if self.max_accel < absolute_acceleration {
            self.max_accel = absolute_acceleration;
        }
        self.downcourse_speed += down_a;
        self.sidecourse_speed += side_a;
		let previous = *self.ski_path.points.last().unwrap();
		self.ski_path.points.push(previous + CoursePoint::create(self.downcourse_speed, self.sidecourse_speed));
    }
}

trait SkierAgent {
	fn get_next_acceleration(&mut self) -> (i32, i32);
	fn revise_prev_accel(&mut self);
	fn duplicate(&self) -> Box<dyn SkierAgent>;
	fn mutate(&mut self);
	fn dump(&mut self);
}

#[derive(Clone)]
struct GeneticSkierAgent {
	call_count: usize,
    moves: [(i8, i8); 4000],
}

impl SkierAgent for GeneticSkierAgent {
	fn get_next_acceleration(&mut self) -> (i32, i32) {
        let (left, right) = self.moves.get(self.call_count).unwrap_or(&(0,0));
		self.call_count += 1;
		(*left as i32, *right as i32)
    }
	fn revise_prev_accel(&mut self) {
		self.call_count -= 1;
		self.inc_mutate_index(self.call_count);
	}
	fn duplicate(&self) -> Box<dyn SkierAgent> {
		Box::new(self.clone())
	}

	fn mutate(&mut self)  {
		self.mutate_index(rand::thread_rng().gen::<usize>() % 4000)
	}
	fn dump(&mut self) {
		for x in 0..self.moves.len() {
			let a = self.moves[x].0;
			let b = self.moves[x].1;
			if a != 0 || b != 0 {
				println!("({x}, {a}, {b}),");
			}
		}
		println!("{}", self.call_count);
	}
}

impl GeneticSkierAgent {
	const MAX_ACCEL:i8 = 3;
	fn craft(accelerations: Vec<(usize, i8, i8)>) -> Self {
		let mut ret = Self::default();
		for (index, a, b) in accelerations {
			if let Some(accel) = ret.moves.get_mut(index) {
				*accel = (a,b);
			}
		}
		ret
	}
	fn mutate_index(&mut self, index: usize) {
		let mut rng = rand::thread_rng();
		self.moves[index] = (rng.gen::<i8>() % Self::MAX_ACCEL, rng.gen::<i8>() % Self::MAX_ACCEL);
	}
	fn inc_mutate_index(&mut self, index: usize) {
		if let Some((a,b)) = self.moves.get_mut(index) {
			*a += 1;
			if *a > Self::MAX_ACCEL {
				*a = -Self::MAX_ACCEL;
				*b += 1;
				if *b > Self::MAX_ACCEL {
					*b = -Self::MAX_ACCEL;
				}
			}
		}
	}

}

impl Default for GeneticSkierAgent {
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
		let mut dt =  DrawTarget::new(size.0 as i32, size.1 as i32);
		dt.clear(SolidSource::from_unpremultiplied_argb(0xff, 0xEE, 0xEE, 0xff));
        Self {
            window,
            size,
            dt,
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
    // fn draw_boat(&mut self, boat_loc: i32) {
    //     let mut pb = PathBuilder::new();
    //     let (side, down) = Self::convert_course_point(CoursePoint::create(boat_loc, 0));
    //     pb.arc(side, down, 4.0, 0., 2. * 3.14159);
    //     let path = pb.finish();
    //     self.dt.fill(&path, &Source::Solid(SolidSource::from_unpremultiplied_argb(0xff, 0, 0xff, 0)), &DrawOptions::new());
    // }
	// fn draw_segment(&mut self, segment: &CourseLineSeg) {
	// 	let mut pb = PathBuilder::new();
	// 	let (px, py) = Self::convert_course_point(segment.0);
	// 	let (qx, qy) = Self::convert_course_point(segment.1);
	// 	let length = qx - px;
	// 	pb.rect(px, py, length, 1.0);
	// 	let path = pb.finish();
	// 	self.dt.fill(&path, &Source::Solid(SolidSource::from_unpremultiplied_argb(0xff, 0, 0xff, 0)), &DrawOptions::new());
	// }
	fn draw_skier(&mut self, skier_location: &SkierPath) {
		for x in skier_location.points.iter() {
			let mut pb = PathBuilder::new();
			let (side, down) = Self::convert_course_point(*x);
            pb.arc(side, down, 0.75, 0., 2. * 3.14159);
			let path = pb.finish();
			self.dt.fill(&path, &Source::Solid(SolidSource::from_unpremultiplied_argb(0xff, 0, 0, 0xff)), &DrawOptions::new());
		}
    }
    fn window_loop(&mut self) {
        if self.is_open() {
            self.window.update_with_buffer(self.dt.get_data(), self.size.0, self.size.1).unwrap();
            self.dt.clear(SolidSource::from_unpremultiplied_argb(0xff, 0xEE, 0xEE, 0xff));
        }
    }
}

impl Clone for Box<dyn SkierAgent> {
    fn clone(&self) -> Self {
        self.duplicate()
    }
}

fn score_skier_agent(skier_agent: &mut dyn SkierAgent, course: &mut SlalomCourse) -> (SkiScore, Skier, i32) {
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
    'main: while boat_location < end_of_course {
        boat_location += 10;
		let mut previous;
		let mut current;
		let mut revision_attempts = 0;
		loop {
			let accel = skier_agent.get_next_acceleration();
			(previous, current) = skier.propose_tic(accel);
			let boat_loc = CoursePoint::create(boat_location, 0);
			let distance: f64 = current.distance_from(&boat_loc);
			let fudge: f64 = distance - f64::from(rope_length);
			if fudge.abs() > skier_reach {
				skier_agent.revise_prev_accel();
				revision_attempts += 1;
				if revision_attempts > 200 {
					break 'main;
				}
			}
			else {
				skier.commit_tic(accel);
				break;
			}
		}
        let segment = CourseLineSeg(previous, current);
        if !entry_gates {
			if boat_location > 2700 {
				break;
			}
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
            if course.buoys[x].death_line.intersects(&segment) {
				break;
			}
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
				break 'main;
			}
        }
    }
    if !entry_gates {
        return (SkiScore::missed_entry(), skier, boat_location);
    }
    for x in 0..5u16 {
        match buoys[x as usize] {
            (true, true, true) => continue,
            (true, true, false) => return (SkiScore::no_exit(x * 100 + 50), skier, boat_location),
            (true, false, _) => return (SkiScore::no_exit(x * 100 + 25), skier, boat_location),
            (false, _, _) => return (SkiScore::no_exit(x), skier, boat_location),
        }
    }
    if exit_gates {
        return (SkiScore::full_pass(), skier, boat_location);
    }
    else {
        return (SkiScore::no_exit(600), skier, boat_location);
    }
}


// fn tests() {
// 	let gates = CourseLineSeg(CoursePoint { downcourse: 0, sidecourse: 125 }, CoursePoint { downcourse: 0, sidecourse: -125 });
// 	let skier = CourseLineSeg(CoursePoint { downcourse: -5, sidecourse: 0 }, CoursePoint { downcourse: 5, sidecourse: 0 });
// 	assert!(gates.intersects(&skier));
// }

fn breed(population: &mut Vec<Box<dyn SkierAgent>>, target_size: usize) {
	let mut new_elems: Vec<Box<dyn SkierAgent>>= vec![Box::new(GeneticSkierAgent::default())];
	for x in population.iter().take(10) {
		new_elems.push(x.duplicate());
	}
	for x in population.iter().take(50) {
		let mut a = x.duplicate();
		a.mutate();
		new_elems.push(a);
	}
	let mut r = rand::thread_rng();
	while new_elems.len() + population.len() < target_size {
		let lucky_index = r.gen::<usize>() % population.len();
		let mut b = population.get(lucky_index).unwrap().duplicate();
		b.mutate();
		b.mutate();
		new_elems.push(b);
	}
	for x in new_elems.iter_mut() {
		x.mutate();
		x.mutate();
		x.mutate();
	}
	population.append(&mut new_elems);
}

fn select(current_population: Vec<Box<dyn SkierAgent>>, limit: usize) -> Vec<Box<dyn SkierAgent>> {
	let mut scored = current_population.into_iter().map(|mut ski_agent| {
		let (score, skier, boat_loc) = score_skier_agent(&mut *ski_agent, &mut SlalomCourse::create());
		(ski_agent, score, skier, boat_loc)
	}).collect::<Vec<(Box<dyn SkierAgent>, SkiScore, Skier, i32)>>();
	scored.shuffle(&mut rand::thread_rng());
	scored.sort_by(|a, b| {
		match (a.1.entry_gates, b.1.entry_gates) {
			(true, false) => return Ordering::Greater,
			(false, true) => return Ordering::Less,
			(false, false) => return Ordering::Equal,
			_ => {},
		}
		if a.1.score == b.1.score {
			match (a.1.exit_gates, b.1.exit_gates) {
				(true, false) => Ordering::Greater,
				(false, true) => Ordering::Less,
				(false, false) | (true, true) => {
					a.3.cmp(&b.3)
				}
			}
		}
		else if a.1.score > b.1.score {
			Ordering::Greater
		}
		else {
			Ordering::Less
		}
	});
	scored.reverse();
	scored.into_iter().map(|e|{
		e.0
	}).take(limit).collect()
}

fn select_and_improve(population: &mut Vec<Box<dyn SkierAgent>>) {
	for x in 0..5 {
	    breed(population, 2000);
        let mut temp = Default::default();
        std::mem::swap(&mut temp, population);
        temp = select(temp, 400);
        std::mem::swap(&mut temp, population);
		println!("{x}");
    }
}

fn main() {
    let mut viewscreen = ViewScreen::create();
    let mut course = SlalomCourse::create();
	// let agent = GeneticSkierAgent::default();
	let agent = GeneticSkierAgent::craft(
		vec![
			(750, 3, 3),
			(766, -3, 1),
			(800, 0, 3),
			(859, 2, 0),
			(860, 1, 0),
			(897, 1, 0),
			(917, 1, 0),
			(941, 1, 0),
			(955, 1, 0),
			(971, 1, 0),
			(980, 1, 0),
			(990, 1, 0),
			(996, 1, 0),
			(1000, -1, -3),
			(1001, -1, -3),
			(1002, -2, -3),
			(1019, -3, 1),
			(1020, -3, 1),
			(1022, -3, 1),
			(1029, -3, -3),
			(1030, -3, -3),
			(1098, 1, 0),
			(1099, 2, 0),
			(1100, 1, -3),
			(1101, 3, -3),
			(1110, 1, 0),
			(1116, -3, 1),
			(1117, -3, 1),
			(1118, -3, -3),
			(1133, -3, -3),
			(1137, 2, -3),
			(1140, 3, -3),
			(1141, 3, -3),
			(1142, 1, -3),
			(1147, -3, 1),
			(1149, -3, 1),
			(1151, 2, -3),
			(1157, -3, -3),
			(1159, 1, -3),
			(1160, 1, -3),
			(1161, -3, -3),

		]
	);
	let mut population: Vec<Box<dyn SkierAgent>> = vec![Box::new(agent)];
    select_and_improve(&mut population);
	let agent = population.first_mut().unwrap();
    let (score, skier, _boat_loc) = score_skier_agent(&mut **agent, &mut course);
    agent.dump();
	println!("score: {score:?}");
	while viewscreen.is_open() {
		sleep(Duration::from_millis(200));
		course.draw_buoys(&mut viewscreen);
		viewscreen.draw_skier(&skier.ski_path);
		viewscreen.window_loop();
    }
}