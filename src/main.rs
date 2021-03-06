use std::collections::BTreeSet;
use std::collections::BTreeMap;
use ordered_float::NotNan;

use pest::{Parser, iterators::Pair};
use pest_derive::*;
use std::fs;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct Grammar;

type InternalFloat = f64;
type IF = InternalFloat;

pub fn main () {
    let args: Vec<String> = std::env::args().collect();

    // read
    let target: String = args.get(1).expect("Please supply a filename in the first argument.").clone();
    let src: String = match fs::read_to_string(&target) {
        Err(_) => panic!("Could not read target file '{}'.", &target),
        Ok(src) => src
    };

    let start: IF =
            args.get(2)
                .expect("Please supply a range lower bound in the second argument.")
                .parse()
                .expect("Please supply a float for the range lower bound in the second argument.");

    let end: IF =
            args.get(3)
                .expect("Please supply a range upper bound in the third argument.")
                .parse()
                .expect("Please supply a float for the range upper bound in the third argument.");

    let base: IF =
            args.get(4)
                .expect("Please supply a log base in the fourth argument.")
                .parse()
                .expect("Please supply a float for the log base in the fourth argument.");

    // parse
    let top_level = Grammar::parse(Rule::top_level, &src).expect("Parse unsuccessful.").next().unwrap();
    let ast_spec: Pair<Rule> = top_level.into_inner().next().unwrap();
    let spec: PartitionSpec = ast_spec_into_spec(ast_spec);
    println!("{:?}", spec);

    // generate marks
    let config = Config {
        minimum_distance: 0.,
        post_transform: Box::new(move |x| x.log(base))
    };
    let marks = spec.run_top(&Interval { start, end, height: 1. }, &config);

    // print JSON for marks
    for tick in &marks.0[0].ticks {
        println!("{}", tick.to_json());
    }
}

pub fn ast_spec_into_spec (spec: Pair<Rule>) -> PartitionSpec {
    assert_eq!(spec.as_rule(), Rule::spec);

    let mut spec_inners = spec.into_inner();
    let spec_tuple = spec_inners.next().unwrap();
    let may_spec_follow = spec_inners.next();

    let mut spec_tuple_inners = spec_tuple.into_inner();
    let spec_tuple_quantities = spec_tuple_inners.next().unwrap();
    let spec_tuple_tick_heights = spec_tuple_inners.next().unwrap();
    let spec_tuple_tick_format = spec_tuple_inners.next();

    let spec_tuple_quantities = spec_tuple_quantities.into_inner().next().unwrap();
    let quantities: Vec<IF> = match spec_tuple_quantities.as_rule() {
        Rule::standalone_quantity => {
            let standalone =
                    spec_tuple_quantities
                        .as_str().parse()
                        .expect("Spec tuple standalone_quantity should have an int.");

            repeat(standalone)
        },
        Rule::multiple_quantities => {
            let mut multiple = vec![];
            for quantity in spec_tuple_quantities.into_inner() {
                let val = quantity.as_str().parse().expect("Spec tuple multiple_quantities should be ints.");
                multiple.push(val);
            }

            multiple
        },
        _ => {
            panic!("Error: quantities was not one of `standalone_quantity` or `multiple_quantities`");
        }
    };

    let base_interval_spec = TickSpec {
        height: spec_tuple_tick_heights
                    .as_str().parse()
                    .expect("Spec tuple should have a float height."),
        format: match spec_tuple_tick_format {
            None => TickFormat::Nothing,
            Some(spec_tuple_tick_format) => {
                let inner = spec_tuple_tick_format.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::debug_format => TickFormat::Debug,
                    Rule::hardcoded_format => TickFormat::Hardcoded(inner.as_str().to_string()),
                    _ => panic!("Error: `format` ast node rule was not one of `debug_format` or `hardcoded_format`")
                }
            }
        }
    };

    let next_specs = match may_spec_follow {
        None => IndexingSpec::Nothing,
        Some(spec_follow) => {
            let spec_follow = spec_follow.into_inner().next().unwrap();
            match spec_follow.as_rule() {
                Rule::split_all => {
                    let sub_spec_ast = spec_follow.into_inner().next().unwrap();
                    let sub_spec = ast_spec_into_spec(sub_spec_ast);
                    IndexingSpec::AllDifferent(Box::new(sub_spec))
                },
                Rule::split_subset => {
                    let mut custom_next_specs = vec![];

                    let mut last_idx = 0;
                    for ranged_spec in spec_follow.into_inner() {
                        let mut inners = ranged_spec.into_inner();

                        let range = inners.next().unwrap();
                        let mut range_start: Option<usize> = None;
                        let mut range_end: Option<usize> = None;
                        for inner in range.into_inner() {
                            match inner.as_rule() {
                                Rule::range_start => {
                                    let s = inner.into_inner().next().unwrap().as_str();
                                    let val = s.parse().unwrap();
                                    range_start = Some(val);
                                },
                                Rule::range_end => {
                                    let s = inner.into_inner().next().unwrap().as_str();
                                    let val = s.parse().unwrap();
                                    range_end = Some(val);
                                },
                                _ => {
                                    panic!("Error: range contains a Pair that is not one of `range_start` or `range_end`");
                                }
                            }
                        }

                        let sub_spec = ast_spec_into_spec(inners.next().unwrap());
                        let range_start = range_start.expect("Error: AST range does not have a start.");
                        let range_end = range_end.unwrap_or(range_start);

                        // check if we need to skip any
                        if range_start - last_idx > 1 {
                            custom_next_specs.push((range_start - last_idx - 1, None))
                        }

                        custom_next_specs.push((1 + range_end - range_start, Some(sub_spec)));

                        last_idx = range_end;
                    }

                    IndexingSpec::Custom(custom_next_specs)
                },
                _ => {
                    panic!("Error: spec_follow was not one of `split_all` or `split_subset`");
                }
            }
        }
    };

    PartitionSpec {
        base_interval_spec, next_specs, quantities,
        override_tick_specs: BTreeMap::new(),
    }
}

pub struct Config {
    minimum_distance: IF,
    post_transform: Box<dyn Fn(IF) -> IF>
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
    label: Option<Label>,
    height: IF,
    offset: TickOffset
}

#[derive(Debug, Clone)]
pub struct Label {
    text: String
}

impl Label {
    fn to_json (&self) -> String {
        format!
          ( "{{ \"text\": \"{}\" }}"
          , self.text
          )
    }
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
          ( "{{ \"pre_pos\": {:.*}, \"post_pos\": {}, \"height\": {}, \"offset\": {}, \"label\": {} }}"
          , 8 - (self.pre_pos.log(10.).ceil() as usize).min(0)
          , self.pre_pos
          , self.post_pos
          , self.meta.height
          , self.meta.offset.to_json()
          , match &self.meta.label {
              None => String::from("null"),
              Some(label) => label.to_json()
            }
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
        let top = self.0.pop().expect("Error: LayeredMarks was empty!");
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
        self.0.last_mut().expect("Error: LayeredMarks was empty!").insert(tick)
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

pub fn not_nan (f: IF) -> NotNan<IF> { NotNan::new(f).expect("not_nan: Input number was NaN!") }

#[derive(Clone, Debug)]
pub struct PartitionSpec {
    quantities: Vec<IF>,
    base_interval_spec: TickSpec,
    override_tick_specs: BTreeMap<usize, TickSpec>,
    next_specs: IndexingSpec
}

pub fn repeat (count: usize) -> Vec<IF> {
    (0..count).map(|_| 1.).collect()
}

#[derive(Clone, Debug)]
pub struct TickSpec {
    height: IF,
    format: TickFormat
}

#[derive(Clone, Debug)]
pub enum TickFormat {
    Nothing,
    Debug,
    Hardcoded(String)
}

impl TickFormat {
    pub fn run (&self, x: IF) -> Option<String> {
        match self {
            TickFormat::Nothing => None,
            TickFormat::Debug => {
                // account for floating-point imprecision
                // TODO: this introduces a whole host of OTHER problems - fix properly...
                Some(format!("{}", (x * 100_000_000.).round() / 100_000_000.))
            },
            TickFormat::Hardcoded(s) => Some(s.clone())
        }
    }
}

#[derive(Clone, Debug)]
pub enum IndexingSpec {
    AllSame(Box<PartitionSpec>),
    AllDifferent(Box<PartitionSpec>),
    Individual(Vec<PartitionSpec>),
    Custom(Vec<(usize, Option<PartitionSpec>)>),
    Nothing
}

impl IndexingSpec {
    fn to_vec (&self, maximum: usize) -> Vec<(usize, Option<PartitionSpec>)> {
        match self {
            IndexingSpec::AllSame(spec)      => vec![(maximum, Some((**spec).clone()))],
            IndexingSpec::AllDifferent(spec) => (0..maximum).map(|_| (1, Some((**spec).clone()))).collect(),
            IndexingSpec::Individual(specs)  => specs.into_iter().map(|spec| (1, Some(spec.clone()))).collect(),
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
                height: self.base_interval_spec.height * interval.height
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
                for (glob_subintervals, next_spec) in self.next_specs.to_vec(self.quantities.len()).iter() {
                    let start_idx = subinterval_idx;
                    subinterval_idx += glob_subintervals;
                    let end_idx = subinterval_idx;

                    if let Some(next_spec) = next_spec {
                        for subinterval in &subintervals[start_idx..end_idx] {
                            next_spec.run((false, false), subinterval, config, committed_marks);
                        }
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

                let spec = self.override_tick_specs.get(&i).unwrap_or(&self.base_interval_spec);

                let label = spec.format.run(point);
                let label = label.map(|text| Label { text });

                let tick_meta = TickMeta {
                    height: interval.height * spec.height,
                    label,
                    offset: TickOffset::Vertical(0.)
                };

                let tick = Tick::new(point, &tick_meta, config);

                if !committed_marks.no_overlap(&tick, config.minimum_distance) { return None; }

                committed_marks.insert(tick);
            }

            // handle end of subinterval
            if last && inclusivity.1 {
                let point = subinterval.end;

                let spec = self.override_tick_specs.get(&(i + 1)).unwrap_or(&self.base_interval_spec);

                let label = spec.format.run(point);
                let label = label.map(|text| Label { text });

                let tick_meta = TickMeta {
                    height: interval.height * spec.height,
                    label,
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
