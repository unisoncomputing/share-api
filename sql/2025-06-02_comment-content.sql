-- Create a View with the latest content for each comment.
CREATE VIEW comment_content(comment_id, created_at, updated_at, content, author_id, contribution_id, ticket_id) AS
  SELECT DISTINCT ON (c.id)
      c.id AS comment_id,
      c.created_at,
      c.updated_at,
      cr.content,
      c.author_id,
      c.contribution_id,
      c.ticket_id
  FROM comments c
  JOIN comment_revisions cr ON c.id = cr.comment_id
  ORDER BY c.id, cr.revision_number DESC
;
