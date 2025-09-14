SELECT * FROM public.lmr_data
ORDER BY lmr_id ASC;

ALTER TABLE lmr_data
	ALTER COLUMN lmr_id DROP IDENTITY;
	
ALTER TABLE public.lmr_data
	ALTER COLUMN lmr_id ADD GENERATED ALWAYS AS IDENTITY;

SELECT MAX(lmr_id) + 1 FROM lmr_data;

DO $$
DECLARE
    next_id bigint;
BEGIN
    -- get max existing value + 1
    SELECT COALESCE(MAX(lmr_id), 0) + 1 INTO next_id
    FROM public.lmr_data;

    -- reset the identity to start from that value
    EXECUTE format(
        'ALTER TABLE public.lmr_data ALTER COLUMN lmr_id RESTART WITH %s',
        next_id
    );
END
$$;

SELECT column_name, column_default, is_identity
FROM information_schema.columns
WHERE table_name = 'lmr_data'
  AND column_name = 'lmr_id';

