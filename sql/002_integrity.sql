-- Integrity triggers. Statements are separated by a line that contains only -- >>>.
CREATE OR REPLACE FUNCTION reject_payment_mutation() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  RAISE EXCEPTION 'payment events are frozen';
END;
$$;
-- >>>
DROP TRIGGER IF EXISTS payment_events_no_update ON payment_events;
-- >>>
CREATE TRIGGER payment_events_no_update
  BEFORE UPDATE OR DELETE ON payment_events
  FOR EACH ROW EXECUTE FUNCTION reject_payment_mutation();
-- >>>
CREATE OR REPLACE FUNCTION reject_sealed_batch_edit() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.status IN ('applied', 'rejected') AND NEW.status IS DISTINCT FROM OLD.status THEN
    RAISE EXCEPTION 'applied and rejected batches are sealed';
  END IF;
  IF OLD.status IN ('applied', 'rejected') AND (
    NEW.total_gbp IS DISTINCT FROM OLD.total_gbp OR NEW.batch_name IS DISTINCT FROM OLD.batch_name
  ) THEN
    RAISE EXCEPTION 'applied and rejected batches are sealed';
  END IF;
  RETURN NEW;
END;
$$;
-- >>>
DROP TRIGGER IF EXISTS financial_batches_sealed ON financial_batches;
-- >>>
CREATE TRIGGER financial_batches_sealed
  BEFORE UPDATE ON financial_batches
  FOR EACH ROW EXECUTE FUNCTION reject_sealed_batch_edit();
