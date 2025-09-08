-- Function for cloud to check org permissions.
CREATE FUNCTION public.user_has_org_permission(user_id UUID, org_user_id UUID, permission permission)
RETURNS BOOLEAN
STABLE
PARALLEL SAFE
AS $$
  SELECT user_has_permission(user_id, (SELECT resource_id FROM orgs o WHERE o.user_id = org_user_id), permission);
$$ LANGUAGE SQL;
