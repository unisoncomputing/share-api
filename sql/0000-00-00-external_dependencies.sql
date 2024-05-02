-- Mocks of external view dependencies
CREATE TABLE public.cloud_subscribers(
  user_id UUID PRIMARY KEY,
  is_active BOOLEAN,
  tier_name TEXT
);

