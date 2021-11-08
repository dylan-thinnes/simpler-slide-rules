use std::collections::BTreeSet;
use std::collections::BTreeMap;
use ordered_float::NotNan;

type InternalFloat = f64;
type IF = InternalFloat;

pub fn main () {
    let mut custom_next_specs = vec![];

    let mut c_spec = PartitionSpec {
        quantities: repeat(9),
        base_interval_height: 0.02,
        override_tick_heights: BTreeMap::new(),
        next_specs: IndexingSpec::Nothing
    };

    let c1_spec = PartitionSpec {
        quantities: repeat(10),
        base_interval_height: 0.8,
        override_tick_heights: BTreeMap::new(),
        next_specs: IndexingSpec::AllSame(Box::new(PartitionSpec {
            quantities: repeat(2),
            base_interval_height: 2./3.,
            override_tick_heights: BTreeMap::new(),
            next_specs: IndexingSpec::AllSame(Box::new(PartitionSpec {
                quantities: repeat(5),
                base_interval_height: 3./4.,
                override_tick_heights: BTreeMap::new(),
                next_specs: IndexingSpec::Nothing
            }))
        }))
    };

    let c2_3_spec = PartitionSpec {
        quantities: repeat(2),
        base_interval_height: 0.8,
        override_tick_heights: BTreeMap::new(),
        next_specs: IndexingSpec::AllSame(Box::new(PartitionSpec {
            quantities: repeat(5),
            base_interval_height: 2./3.,
            override_tick_heights: BTreeMap::new(),
            next_specs: IndexingSpec::AllSame(Box::new(PartitionSpec {
                quantities: repeat(5),
                base_interval_height: 3./4.,
                override_tick_heights: BTreeMap::new(),
                next_specs: IndexingSpec::Nothing
            }))
        }))
    };

    let c4_9_spec = PartitionSpec {
        quantities: repeat(2),
        base_interval_height: 0.8,
        override_tick_heights: BTreeMap::new(),
        next_specs: IndexingSpec::AllSame(Box::new(PartitionSpec {
            quantities: repeat(5),
            base_interval_height: 2./3.,
            override_tick_heights: BTreeMap::new(),
            next_specs: IndexingSpec::AllSame(Box::new(PartitionSpec {
                quantities: repeat(2),
                base_interval_height: 3./4.,
                override_tick_heights: BTreeMap::new(),
                next_specs: IndexingSpec::Nothing
            }))
        }))
    };

    custom_next_specs.push((1, c1_spec));
    custom_next_specs.push((2, c2_3_spec));
    custom_next_specs.push((6, c4_9_spec));
    c_spec.next_specs = IndexingSpec::Custom(custom_next_specs);

    let config = Config {
        minimum_distance: 0.,
        post_transform: |x| {
            x.log(10.)
        }
    };

    let marks = c_spec.run_top(&Interval { start: 1., end: 10., height: 1. }, &config);
    for tick in &marks.0[0].ticks {
        println!("{}", tick.to_json());
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
    height: IF,
    offset: TickOffset
}

#[derive(Debug, Clone)]
pub enum TickOffset {
    Radial(IF),
    Vertical(IF)
}

impl TickOffset {
    fn to_json (&self) -> String {
        let tag = match self {
            TickOffset::Radial(_) => "Radial",
            TickOffset::Vertical(_) => "Vertical"
        };

        let value = match self {
            TickOffset::Radial(f) => f,
            TickOffset::Vertical(f) => f
        };

        format!
          ( "{{ \"tag\": \"{}\", \"contents\": {} }}"
          , tag
          , value
          )
    }
}

impl Tick {
    fn new (pre_pos: IF, meta: &TickMeta, config: &Config) -> Self {
        let post_pos = (config.post_transform)(pre_pos);

        Tick {
            pre_pos, post_pos,
            meta: meta.clone()
        }
    }

    fn to_json (&self) -> String {
        format!
          ( "{{ \"pre_pos\": {}, \"post_pos\": {}, \"height\": {}, \"offset\": {} }}"
          , self.pre_pos
          , self.post_pos
          , self.meta.height
          , self.meta.offset.to_json()
          )
    }
}

#[derive(Debug)]
pub struct LayeredMarks(Vec<Marks>);

impl LayeredMarks {
    fn new () -> Self {
        let mut s = LayeredMarks(vec![]);
        s.new_layer();
        return s;
    }

    fn new_layer (&mut self) {
        self.0.push(Marks::new());
    }

    fn collapse_layer (&mut self) {
        let top = self.0.pop().unwrap();
        match self.0.last_mut() {
            None => {
                eprintln!("Warning: Tried to collapse only layer on LayeredMarks stack!");
                self.0.push(top);
            },
            Some(new_top) => new_top.merge(top)
        }
    }

    fn discard_layer (&mut self) {
        if self.0.len() > 1 {
            self.0.pop();
        }
    }

    fn insert (&mut self, tick: Tick) -> bool {
        self.0.last_mut().unwrap().insert(tick)
    }

    pub fn no_overlap (&self, tick: &Tick, minimum_distance: InternalFloat) -> bool {
        let point = tick.post_pos;
        let (le, ge) = self.closest_gle(point);

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
        let mut closest_le: Option<IF> = None;
        let mut closest_ge: Option<IF> = None;

        for layer in &self.0 {
            let (le, ge) = layer.closest_gle(point);

            closest_le = match (closest_le, le) {
                (None, _) => le,
                (_, None) => closest_le,
                (Some(closest_le), Some(le)) => Some(closest_le.max(le))
            };

            closest_ge = match (closest_ge, ge) {
                (None, _) => ge,
                (_, None) => closest_ge,
                (Some(closest_ge), Some(ge)) => Some(closest_ge.min(ge))
            };
        }

        return (closest_le, closest_ge);
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

    pub fn no_overlap (&self, tick: &Tick, minimum_distance: InternalFloat) -> bool {
        let point = tick.post_pos;
        let (le, ge) = self.closest_gle(point);

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

#[derive(Clone)]
pub struct PartitionSpec {
    quantities: Vec<IF>,
    base_interval_height: IF,
    override_tick_heights: BTreeMap<usize, IF>,
    next_specs: IndexingSpec
}

pub fn repeat (count: usize) -> Vec<IF> {
    (0..count).map(|_| 1.).collect()
}

#[derive(Clone)]
pub enum IndexingSpec {
    AllSame(Box<PartitionSpec>),
    AllDifferent(Box<PartitionSpec>),
    Individual(Vec<PartitionSpec>),
    Custom(Vec<(usize, PartitionSpec)>),
    Nothing
}

impl IndexingSpec {
    fn to_vec (&self, maximum: usize) -> Vec<(usize, PartitionSpec)> {
        match self {
            IndexingSpec::AllSame(spec)      => vec![(maximum, (**spec).clone())],
            IndexingSpec::AllDifferent(spec) => (0..maximum).map(|_| (1, (**spec).clone())).collect(),
            IndexingSpec::Individual(specs)  => specs.into_iter().map(|spec| (1, spec.clone())).collect(),
            IndexingSpec::Custom(specs)      => specs.to_vec(),
            IndexingSpec::Nothing            => vec![]
        }
    }
}

impl PartitionSpec {
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

    pub fn run_top (&self, interval: &Interval, config: &Config) -> LayeredMarks {
        let mut committed_marks = LayeredMarks::new();
        let inclusivity = (true, true);

        self.run(inclusivity, interval, config, &mut committed_marks);
        return committed_marks;
    }

    pub fn run (&self, inclusivity: (bool, bool), interval: &Interval, config: &Config, committed_marks: &mut LayeredMarks) -> bool {
        let attempt_result = self.attempt(inclusivity, interval, config, committed_marks);

        match attempt_result {
            None => {
                committed_marks.discard_layer();

                return false;
            },
            Some(subintervals) => {
                committed_marks.collapse_layer();

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

    pub fn attempt (&self, inclusivity: (bool, bool), interval: &Interval, config: &Config, committed_marks: &mut LayeredMarks) -> Option<Vec<Interval>> {
        committed_marks.new_layer();
        let subintervals = self.partition(interval);

        for (i, subinterval) in subintervals.iter().enumerate() {
            let first: bool = i == 0;
            let last: bool = i == subintervals.len() - 1;

            // handle front of subinterval
            if !first || inclusivity.0 {
                let point = subinterval.start;

                let overridden_height = match self.override_tick_heights.get(&i) {
                    Some(factor) => interval.height * factor,
                    None => interval.height * self.base_interval_height
                };

                let tick_meta = TickMeta {
                    height: overridden_height,
                    label: None,
                    offset: TickOffset::Vertical(0.)
                };

                let tick = Tick::new(point, &tick_meta, config);

                if !committed_marks.no_overlap(&tick, config.minimum_distance) { return None; }

                committed_marks.insert(tick);
            }

            // handle end of subinterval
            if last && inclusivity.1 {
                let point = subinterval.end;

                let overridden_height = match self.override_tick_heights.get(&(i + 1)) {
                    Some(factor) => interval.height * factor,
                    None => interval.height * self.base_interval_height
                };

                let tick_meta = TickMeta {
                    height: overridden_height,
                    label: None,
                    offset: TickOffset::Vertical(0.)
                };

                let tick = Tick::new(point, &tick_meta, config);

                if !committed_marks.no_overlap(&tick, config.minimum_distance) { return None; }

                committed_marks.insert(tick);
            }
        }

        return Some(subintervals);
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
