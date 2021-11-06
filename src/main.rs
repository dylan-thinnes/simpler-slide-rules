use std::collections::BTreeSet;
use std::collections::BTreeMap;
use ordered_float::NotNan;

type InternalFloat = f64;
type IF = InternalFloat;

pub fn main () {
    let mut fifth_tick_higher = BTreeMap::new();
    fifth_tick_higher.insert(5, 0.75);

    let div10_spec_ = PartitionSpec {
        quantities: repeat(10),
        base_interval_height: 0.5,
        override_tick_heights: &BTreeMap::new(),
        next_specs: IndexingSpec::Nothing
    };

    let div10_spec = PartitionSpec {
        quantities: repeat(10),
        base_interval_height: 0.5,
        override_tick_heights: &fifth_tick_higher,
        next_specs: IndexingSpec::AllSame(&div10_spec_)
    };

    let c_spec = PartitionSpec {
        quantities: repeat(9),
        base_interval_height: 0.1,
        override_tick_heights: &BTreeMap::new(),
        next_specs: IndexingSpec::AllDifferent(&div10_spec)
    };

    let config = Config {
        minimum_distance: 0.,
        post_transform: |x| {
            x.log(10.)
        }
    };

    let marks = c_spec.run_top(&Interval { start: 1., end: 10., height: 1. }, &config);
    for tick in marks.ticks {
        tick.to_json();
    }
}

pub struct Config {
    minimum_distance: IF,
    post_transform: fn(IF) -> IF
}

#[derive(Debug)]
pub struct Interval {
    start: IF,
    end: IF,
    height: IF
}

#[derive(Debug)]
pub struct Tick {
    pre_pos: IF,
    post_pos: IF,
    meta: TickMeta
}

#[derive(Debug, Clone)]
pub struct TickMeta {
    label: Option<String>,
    height: IF
}

impl Tick {
    fn new (pre_pos: IF, meta: &TickMeta, config: &Config) -> Self {
        let post_pos = (config.post_transform)(pre_pos);

        Tick {
            pre_pos, post_pos,
            meta: meta.clone()
        }
    }

    fn to_json (&self) {
        println!
          ( "{{ \"pre_pos\": {}, \"post_pos\": {}, \"height\": {} }}"
          , self.pre_pos
          , self.post_pos
          , self.meta.height
          )
    }
}

#[derive(Debug)]
pub struct Marks {
    occupied_positions: BTreeSet<NotNan<IF>>,
    ticks: Vec<Tick>
}

impl Marks {
    fn new () -> Self {
        Marks {
            occupied_positions: BTreeSet::new(),
            ticks: vec![]
        }
    }

    fn insert (&mut self, tick: Tick) -> bool {
        let insertion_successful = self.occupied_positions.insert(not_nan(tick.post_pos));
        let already_occupied = !insertion_successful;

        if already_occupied {
            eprintln!("Position x: {}, post: {} was already occupied!", tick.pre_pos, tick.post_pos);
        }

        self.ticks.push(tick);

        return already_occupied;
    }

    fn merge (&mut self, other: Self) {
        for x_notnan in other.occupied_positions {
            self.occupied_positions.insert(x_notnan);
        }

        for tick in other.ticks {
            self.ticks.push(tick);
        }
    }

    pub fn no_overlap (&self, point: IF, minimum_distance: InternalFloat) -> bool {
        let (ge, le) = self.closest_gle(point);

        if let Some(le) = le {
            let le_delta = (le - point).abs();
            if minimum_distance > le_delta {
                return false;
            }
        }

        if let Some(ge) = ge {
            let ge_delta = (ge - point).abs();
            if minimum_distance > ge_delta {
                return false;
            }
        }

        return true;
    }

    pub fn closest_gle (&self, point: IF) -> (Option<IF>, Option<IF>) {
        let le_notnan = self.occupied_positions.range(..=not_nan(point)).next_back();
        let ge_notnan = self.occupied_positions.range(not_nan(point)..).next();

        let le = le_notnan.map(|x| x.into_inner());
        let ge = ge_notnan.map(|x| x.into_inner());

        return (le, ge);
    }
}

pub fn not_nan (f: IF) -> NotNan<IF> { NotNan::new(f).unwrap() }

pub struct PartitionSpec<'a> {
    quantities: Vec<IF>,
    base_interval_height: IF,
    override_tick_heights: &'a BTreeMap<usize, IF>,
    next_specs: IndexingSpec<'a>
}

pub fn repeat (count: usize) -> Vec<IF> {
    (0..count).map(|_| 1.).collect()
}

pub enum IndexingSpec<'a> {
    AllSame(&'a PartitionSpec<'a>),
    AllDifferent(&'a PartitionSpec<'a>),
    Individual(Vec<&'a PartitionSpec<'a>>),
    Custom(Vec<(usize, &'a PartitionSpec<'a>)>),
    Nothing
}

impl<'a> IndexingSpec<'a> {
    fn to_vec (&'a self, maximum: usize) -> Vec<(usize, &'a PartitionSpec<'a>)> {
        match self {
            IndexingSpec::AllSame(spec)      => vec![(maximum, spec)],
            IndexingSpec::AllDifferent(spec) => (0..maximum).map(|_| (1, *spec)).collect(),
            IndexingSpec::Individual(specs)  => specs.into_iter().map(|&spec| (1, spec)).collect(),
            IndexingSpec::Custom(specs)      => specs.to_vec(),
            IndexingSpec::Nothing            => vec![]
        }
    }
}

impl PartitionSpec<'_> {
    pub fn partition (&self, interval: &Interval) -> Vec<Interval> {
        let increment = (interval.end - interval.start) / self.quantities.iter().sum::<IF>();
        let mut subintervals = vec![];
        let mut point = interval.start;

        for quantity in self.quantities.iter() {
            let old_point = point;
            point += quantity * increment;

            let subinterval = Interval {
                start: old_point,
                end: point,
                height: self.base_interval_height * interval.height
            };

            subintervals.push(subinterval);
        }

        return subintervals;
    }

    pub fn run_top (&self, interval: &Interval, config: &Config) -> Marks {
        let mut committed_marks = Marks::new();
        let inclusivity = (true, true);

        self.run(inclusivity, interval, config, &mut committed_marks);
        return committed_marks;
    }

    pub fn run (&self, inclusivity: (bool, bool), interval: &Interval, config: &Config, committed_marks: &mut Marks) -> bool {
        let attempt_result = self.attempt(inclusivity, interval, config, committed_marks);

        match attempt_result {
            None => {
                return false;
            },
            Some((local_marks, subintervals)) => {
                committed_marks.merge(local_marks);

                let mut subinterval_idx: usize = 0;
                for (i, (glob_subintervals, next_spec)) in self.next_specs.to_vec(self.quantities.len()).iter().enumerate() {
                    let is_first = i == 0;

                    let start_idx = subinterval_idx;
                    subinterval_idx += glob_subintervals;
                    let end_idx = subinterval_idx;

                    for subinterval in &subintervals[start_idx..end_idx] {
                        next_spec.run((false, false), subinterval, config, committed_marks);
                    }
                }

                return true;
            }
        }
    }

    pub fn attempt (&self, inclusivity: (bool, bool), interval: &Interval, config: &Config, committed_marks: &Marks) -> Option<(Marks, Vec<Interval>)> {
        let mut local_marks: Marks = Marks::new();
        let subintervals = self.partition(interval);

        for (i, subinterval) in subintervals.iter().enumerate() {
            let first: bool = i == 0;
            let last: bool = i == subintervals.len() - 1;

            // handle front of subinterval
            if !first || inclusivity.0 {
                let point = subinterval.start;

                if !committed_marks.no_overlap(point, config.minimum_distance) { return None; }
                if !local_marks.no_overlap(point, config.minimum_distance) { return None; }

                let overridden_height = match self.override_tick_heights.get(&i) {
                    Some(factor) => interval.height * factor,
                    None => interval.height * self.base_interval_height
                };

                let tick_meta = TickMeta {
                    height: overridden_height,
                    label: None
                };

                let tick = Tick::new(point, &tick_meta, config);
                local_marks.insert(tick);
            }

            // handle end of subinterval
            if last && inclusivity.1 {
                let point = subinterval.end;

                if !committed_marks.no_overlap(point, config.minimum_distance) { return None; }
                if !local_marks.no_overlap(point, config.minimum_distance) { return None; }

                let overridden_height = match self.override_tick_heights.get(&(i + 1)) {
                    Some(factor) => interval.height * factor,
                    None => interval.height * self.base_interval_height
                };

                let tick_meta = TickMeta {
                    height: overridden_height,
                    label: None
                };
                let tick = Tick::new(point, &tick_meta, config);
                local_marks.insert(tick);
            }
        }

        return Some((local_marks, subintervals));
    }
}

// Function pointers
pub enum Transformation {
    Offset(IF),
    Scale(IF),
    OffsetThenScale(IF, IF),
    Custom(Box<dyn Fn(IF) -> IF>)
}

impl Transformation {
    pub fn run (&self, x: IF) -> IF {
        match self {
            Transformation::Offset(offset) => x + offset,
            Transformation::Scale(scale) => x * scale,
            Transformation::OffsetThenScale(offset, scale) => offset + x * scale,
            Transformation::Custom(f) => f(x)
        }
    }
}

pub fn run_all (ts: &Vec<Transformation>, x: IF) -> IF {
    let mut y = x;

    for t in ts {
        y = t.run(y)
    }

    return y;
}
