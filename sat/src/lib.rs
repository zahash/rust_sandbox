// conjunctive normal form = (x1 V x2) /\ (~x3 V x4)

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

#[cfg(test)]
mod tests {
    use super::*;
    use utils::read_json;

    #[test]
    fn test() {
        // let expr = vec![vec![-1, 2], vec![-1, 3]];

        // https://www.borealisai.com/research-blogs/tutorial-10-sat-solvers-ii-algorithms/#:~:text=Worked%20example%3A%C2%A0This%20process%20is%20easiest%20to%20understand%20using%20a%20concrete%20example.%C2%A0Consider%20the%20following%202%2DSAT%20problem%20in%20four%20variables%3A
        // let expr = vec![
        //     vec![1, -2],
        //     vec![-1, -3],
        //     vec![2, 3],
        //     vec![-2, 4],
        //     vec![3, -4],
        // ];

        // let expr = vec![
        //     vec![1, 2],
        //     vec![1, -2, -3, 4],
        //     vec![1, -3, -4],
        //     vec![-1, 2, -3],
        //     vec![-1, 2, -4],
        //     vec![-1, 3, 4],
        //     vec![-2, 3],
        // ];

        let expr: CNF = read_json(
            "/home/zahash/storage/sat/CBS_k3_n100_m449_b90_json/CBS_k3_n100_m449_b90_1.json",
        );
        let res = solve(&expr);

        println!("expr = {:?}", expr);
        println!("res  = {:?}", res);

        if let Some(sol) = res {
            println!("check = {}", check(&expr, &sol));
        }
    }
}
