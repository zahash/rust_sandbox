use sqlx::{sqlite::SqlitePool, Pool, Sqlite};

pub async fn sandbox_pool() -> Pool<Sqlite> {
    SqlitePool::connect("sqlite:assets/sandbox.db")
        .await
        .unwrap()
}

pub async fn add_todo(pool: &SqlitePool, description: String) -> Result<i64, sqlx::Error> {
    Ok(sqlx::query!(
        r#"
            INSERT INTO todos ( description ) 
            VALUES ( ?1 ) 
        "#,
        description
    )
    .execute(pool)
    .await?
    .last_insert_rowid())
}

pub async fn complete_todo(pool: &SqlitePool, id: i64) -> Result<(), sqlx::Error> {
    let rows_affected = sqlx::query!(
        r#"
            UPDATE todos
            SET done = TRUE
            WHERE id = ?1
        "#,
        id
    )
    .execute(pool)
    .await?;

    Ok(())
}

pub async fn list_todos(pool: &SqlitePool) -> Result<(), sqlx::Error> {
    let recs = sqlx::query!(
        r#"
            SELECT id, description, done
            FROM todos
            ORDER BY id
        "#
    )
    .fetch_all(pool)
    .await?;

    for rec in recs {
        println!(
            "- [{}] {}: {}",
            if rec.done { "x" } else { " " },
            rec.id,
            &rec.description,
        );
    }

    Ok(())
}
