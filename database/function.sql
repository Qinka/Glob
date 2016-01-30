




-- database/database.sql

CREATE OR REPLACE FUNCTION
  drop_all_table(user_name IN VARCHAR,schema_name IN VARCHAR)
  RETURNS VOID
  AS $$
  DECLARE statements CURSOR FOR
    SELECT tablename FROM pg_tables
    WHERE tableowner = user_name AND
          schemaname = schema_name;
    BEGIN
      FOR stmt IN statements LOOP
        EXECUTE 'DROP TABLE ' || quote_ident(stmt.tablename) || ' CASCADE;';
      END LOOP;
    END;
$$ LANGUAGE plpgsql;
