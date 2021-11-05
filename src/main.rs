use std::collections::BTreeSet;
use ordered_float::NotNan;

type InternalFloat = f64;
type IF = InternalFloat;

pub fn main () {
    let div5_spec = PartitionSpec {
        quantities: vec![1.,1.,1.,1.,1.],
        next_specs: IndexingSpec::Nothing
    };

    let div2_spec = PartitionSpec {
        quantities: vec![1.,1.],
        next_specs: IndexingSpec::AllSame(&div5_spec)
    };

    let c_spec = PartitionSpec {
        quantities: repeat(9),
        next_specs: IndexingSpec::AllDifferent(&div2_spec)
    };

    let marks = c_spec.run_top(&Interval { start: 1., end: 10. }, 0.0001);
    println!("{:?}", marks);
}

#[derive(Debug)]
pub struct Interval {
    start: IF,
    end: IF
}

type Marks = BTreeSet<NotNan<IF>>;
pub fn not_nan (f: IF) -> NotNan<IF> { NotNan::new(f).unwrap() }

pub fn no_overlap (marks: &Marks, point: IF, minimum_distance: InternalFloat) -> bool {
    let (ge, le) = closest_gle(marks, point);

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

pub fn closest_gle (marks: &Marks, point: IF) -> (Option<IF>, Option<IF>) {
    let le_notnan = marks.range(..=not_nan(point)).next_back();
    let ge_notnan = marks.range(not_nan(point)..).next();

    let le = le_notnan.map(|x| x.into_inner());
    let ge = ge_notnan.map(|x| x.into_inner());

    return (le, ge);
}

pub struct PartitionSpec<'a> {
    quantities: Vec<IF>,
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
    pub fn partition (&self, range: &Interval) -> Vec<Interval> {
        let increment = (range.end - range.start) / self.quantities.iter().sum::<IF>();
        let mut subranges = vec![];
        let mut point = range.start;

        for quantity in self.quantities.iter() {
            let old_point = point;
            point += quantity * increment;
            subranges.push(Interval { start: old_point, end: point });
        }

        return subranges;
    }

    pub fn run_top (&self, range: &Interval, minimum_distance: IF) -> Marks {
        let mut committed_marks = BTreeSet::new();
        let inclusivity = (true, true);

        self.run(inclusivity, range, minimum_distance, &mut committed_marks);
        return committed_marks;
    }

    pub fn run (&self, inclusivity: (bool, bool), range: &Interval, minimum_distance: IF, committed_marks: &mut Marks) -> bool {
        let attempt_result = self.attempt(inclusivity, range, minimum_distance, committed_marks);

        match attempt_result {
            None => {
                return false;
            },
            Some((local_marks, subranges)) => {
                for mark in local_marks {
                    committed_marks.insert(mark);
                }

                let mut subrange_idx: usize = 0;
                for (i, (glob_subranges, next_spec)) in self.next_specs.to_vec(self.quantities.len()).iter().enumerate() {
                    let is_first = i == 0;

                    let start_idx = subrange_idx;
                    subrange_idx += glob_subranges;
                    let end_idx = subrange_idx;

                    for subrange in &subranges[start_idx..end_idx] {
                        next_spec.run((!is_first, false), subrange, minimum_distance, committed_marks);
                    }

                }

                return true;
            }
        }
    }

    pub fn attempt (&self, inclusivity: (bool, bool), range: &Interval, minimum_distance: IF, committed_marks: &Marks) -> Option<(Marks, Vec<Interval>)> {
        let mut local_marks: Marks = BTreeSet::new();
        let subranges = self.partition(range);

        for (i, subrange) in subranges.iter().enumerate() {
            let first: bool = i == 0;
            let last: bool = i == subranges.len() - 1;

            // handle front of subrange
            if !first || inclusivity.0 {
                let point = subrange.start;

                if !no_overlap(committed_marks, point, minimum_distance) { return None; }
                if !no_overlap(&local_marks, point, minimum_distance) { return None; }

                local_marks.insert(not_nan(point));
            }

            // handle end of subrange
            if last && inclusivity.1 {
                let point = subrange.end;

                if !no_overlap(committed_marks, point, minimum_distance) { return None; }
                if !no_overlap(&local_marks, point, minimum_distance) { return None; }

                local_marks.insert(not_nan(point));
            }
        }

        return Some((local_marks, subranges));
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
