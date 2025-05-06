-- Delete all previously-computed namespace diffs, because the diff payload is different now (we explicitly store
-- errors).
TRUNCATE namespace_diffs;
