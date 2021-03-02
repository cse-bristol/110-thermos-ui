CREATE OR REPLACE VIEW ranked_jobs AS
SELECT id,state,queue_name,
       row_number()
        OVER (PARTITION BY queue_name,
              state in ('ready')
              ORDER BY queued)
       as queue_position
FROM jobs;
