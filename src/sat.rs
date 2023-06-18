// conjunctive normal form = (x1 V x2) /\ (~x3 V x4)

use std::{fs::File, io::BufReader, path::Path};

type Var = isize;
type Clause = Vec<Var>;
type CNF = Vec<Clause>;

pub fn solve(expr: &CNF) -> Option<Vec<Var>> {
    match expr.iter().flatten().map(|v| v.abs()).next() {
        None => Some(vec![]),

        Some(var) => {
            if let Some((conditioned_expr, resolved_vars)) = condition(expr, var) {
                if let Some(next_resolved_vars) = solve(&conditioned_expr) {
                    let mut all_resolved_vars = vec![];
                    all_resolved_vars.extend(resolved_vars);
                    all_resolved_vars.extend(next_resolved_vars);
                    return Some(all_resolved_vars);
                }
            }

            if let Some((conditioned_expr, resolved_vars)) = condition(expr, inv(&var)) {
                if let Some(next_resolved_vars) = solve(&conditioned_expr) {
                    let mut all_resolved_vars = vec![];
                    all_resolved_vars.extend(resolved_vars);
                    all_resolved_vars.extend(next_resolved_vars);
                    return Some(all_resolved_vars);
                }
            }

            None
        }
    }
}

pub fn check(expr: &CNF, sol: &Vec<Var>) -> bool {
    let mut expr = expr.clone();

    for var in sol {
        expr.push(vec![*var]);
    }

    unit_prop(&mut expr).is_some()
}

fn condition(expr: &CNF, var: Var) -> Option<(CNF, Vec<Var>)> {
    let mut conditioned = expr.clone();
    conditioned.push(vec![var]);
    unit_prop(&mut conditioned).map(|resolved_vars| (conditioned, resolved_vars))
}

fn unit_prop(expr: &mut CNF) -> Option<Vec<Var>> {
    let mut resolved_vars = vec![];

    while let Some(unit_var) = unit_res(expr) {
        if resolved_vars.contains(&inv(&unit_var)) {
            return None;
        }
        resolved_vars.push(unit_var);
    }

    Some(resolved_vars)
}

fn unit_res(expr: &mut CNF) -> Option<Var> {
    match expr
        .iter()
        .find(|clause| clause.len() == 1)
        .map(|clause| *clause.first().unwrap())
    {
        None => None,
        Some(unit_var) => {
            expr.retain(|clause| !clause.contains(&unit_var));
            expr.iter_mut()
                .filter(|clause| clause.len() > 1)
                .for_each(|clause| clause.retain(|var| var != &inv(&unit_var)));
            Some(unit_var)
        }
    }
}

fn inv(var: &Var) -> Var {
    -var
}

pub fn read_json_expr<P>(path: P) -> CNF
where
    P: AsRef<Path>,
{
    let file = File::open(path).unwrap();
    let reader = BufReader::new(file);
    serde_json::from_reader::<BufReader<File>, CNF>(reader).unwrap()
}
