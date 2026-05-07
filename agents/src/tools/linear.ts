/**
 * Linear custom tool handlers.
 *
 * Tool names match the MCP tool names used in claude/skills/ SKILL.md allowed-tools
 * frontmatter, so skills run unmodified in Managed Agents mode.
 *
 * Each handler is async (input: Record<string, unknown>) => Promise<unknown>.
 * The session runner dispatches agent.custom_tool_use events to these by name.
 */

type ToolInput = Record<string, unknown>;
type ToolHandler = (input: ToolInput) => Promise<unknown>;

async function gql(
  apiKey: string,
  query: string,
  variables: Record<string, unknown> = {},
): Promise<unknown> {
  const res = await fetch('https://api.linear.app/graphql', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      Authorization: `Bearer ${apiKey}`,
    },
    body: JSON.stringify({ query, variables }),
  });
  if (!res.ok) throw new Error(`Linear HTTP ${res.status}: ${await res.text()}`);
  const body = (await res.json()) as { data: unknown; errors?: unknown[] };
  if (body.errors?.length) throw new Error(`Linear GQL errors: ${JSON.stringify(body.errors)}`);
  return body.data;
}

export function createLinearToolHandlers(apiKey: string): Record<string, ToolHandler> {
  const q = (query: string, vars?: Record<string, unknown>) => gql(apiKey, query, vars);

  return {
    mcp__claude_ai_Linear__get_issue: ({ id }) =>
      q(`query($id:String!){issue(id:$id){id identifier title description priority
          state{id name type} assignee{id name email} team{id name key}
          project{id name} labels{nodes{id name color}} createdAt updatedAt}}`,
        { id }),

    mcp__claude_ai_Linear__list_issues: ({ teamId, projectId, limit = 25 }) => {
      const filter: Record<string, unknown> = {};
      if (teamId) filter['team'] = { id: { eq: teamId } };
      if (projectId) filter['project'] = { id: { eq: projectId } };
      return q(
        `query($filter:IssueFilter,$first:Int){issues(filter:$filter,first:$first){
            nodes{id identifier title priority state{name} assignee{name} updatedAt}}}`,
        { filter, first: limit },
      );
    },

    mcp__claude_ai_Linear__save_issue: ({ id, ...input }) =>
      id
        ? q(`mutation($id:String!,$input:IssueUpdateInput!){issueUpdate(id:$id,input:$input){success}}`,
            { id, input })
        : q(`mutation($input:IssueCreateInput!){issueCreate(input:$input){success issue{id identifier url}}}`,
            { input }),

    mcp__claude_ai_Linear__list_comments: ({ issueId }) =>
      q(`query($id:String!){issue(id:$id){comments{nodes{id body createdAt author{name}}}}}`,
        { id: issueId }),

    mcp__claude_ai_Linear__save_comment: ({ issueId, body, id }) =>
      id
        ? q(`mutation($id:String!,$body:String!){commentUpdate(id:$id,input:{body:$body}){success}}`,
            { id, body })
        : q(`mutation($issueId:String!,$body:String!){commentCreate(input:{issueId:$issueId,body:$body}){success comment{id}}}`,
            { issueId, body }),

    mcp__claude_ai_Linear__list_teams: () =>
      q(`{teams{nodes{id name key}}}`),

    mcp__claude_ai_Linear__get_team: ({ id }) =>
      q(`query($id:String!){team(id:$id){id name key
          states{nodes{id name type position}} labels{nodes{id name color}}}}`,
        { id }),

    mcp__claude_ai_Linear__list_projects: ({ teamId }) => {
      const filter = teamId ? { teams: { id: { eq: teamId } } } : {};
      return q(
        `query($filter:ProjectFilter){projects(filter:$filter){nodes{id name slugId}}}`,
        { filter },
      );
    },

    mcp__claude_ai_Linear__get_project: ({ id }) =>
      q(`query($id:String!){project(id:$id){id name slugId description}}`, { id }),

    mcp__claude_ai_Linear__list_issue_statuses: ({ teamId }) =>
      q(`query($id:String!){team(id:$id){states{nodes{id name type position}}}}`,
        { id: teamId }),

    mcp__claude_ai_Linear__list_issue_labels: ({ teamId }) =>
      q(`query($id:String!){team(id:$id){labels{nodes{id name color}}}}`,
        { id: teamId }),

    mcp__claude_ai_Linear__list_documents: ({ projectId }) => {
      const filter = projectId ? { project: { id: { eq: projectId } } } : {};
      return q(
        `query($filter:DocumentFilter){documents(filter:$filter){nodes{id title updatedAt}}}`,
        { filter },
      );
    },

    mcp__claude_ai_Linear__get_document: ({ id }) =>
      q(`query($id:String!){document(id:$id){id title content updatedAt}}`, { id }),

    mcp__claude_ai_Linear__create_document: ({ projectId, title, content }) =>
      q(`mutation($input:DocumentCreateInput!){documentCreate(input:$input){success document{id title}}}`,
        { input: { projectId, title, content } }),
  };
}

/** JSON Schema definitions for registering custom tools on an agent. */
export const linearToolSchemas: Array<{ name: string; description: string; input_schema: object }> = [
  {
    name: 'mcp__claude_ai_Linear__get_issue',
    description: 'Retrieve a Linear issue by ID or identifier (e.g. VID-123)',
    input_schema: {
      type: 'object',
      properties: { id: { type: 'string', description: 'Issue ID or identifier' } },
      required: ['id'],
    },
  },
  {
    name: 'mcp__claude_ai_Linear__list_issues',
    description: 'List Linear issues, optionally filtered by team or project',
    input_schema: {
      type: 'object',
      properties: {
        teamId:    { type: 'string' },
        projectId: { type: 'string' },
        limit:     { type: 'number', default: 25 },
      },
    },
  },
  {
    name: 'mcp__claude_ai_Linear__save_issue',
    description: 'Create (no id) or update (with id) a Linear issue',
    input_schema: {
      type: 'object',
      properties: {
        id:          { type: 'string', description: 'Omit to create' },
        title:       { type: 'string' },
        description: { type: 'string' },
        teamId:      { type: 'string' },
        stateId:     { type: 'string' },
        priority:    { type: 'number' },
      },
    },
  },
  {
    name: 'mcp__claude_ai_Linear__list_comments',
    description: 'List comments on a Linear issue',
    input_schema: {
      type: 'object',
      properties: { issueId: { type: 'string' } },
      required: ['issueId'],
    },
  },
  {
    name: 'mcp__claude_ai_Linear__save_comment',
    description: 'Create (no id) or update (with id) a comment on a Linear issue',
    input_schema: {
      type: 'object',
      properties: {
        issueId: { type: 'string' },
        body:    { type: 'string' },
        id:      { type: 'string', description: 'Omit to create' },
      },
      required: ['body'],
    },
  },
  {
    name: 'mcp__claude_ai_Linear__list_teams',
    description: 'List all Linear teams',
    input_schema: { type: 'object', properties: {} },
  },
  {
    name: 'mcp__claude_ai_Linear__get_team',
    description: 'Get a Linear team including its workflow states and labels',
    input_schema: {
      type: 'object',
      properties: { id: { type: 'string' } },
      required: ['id'],
    },
  },
  {
    name: 'mcp__claude_ai_Linear__list_projects',
    description: 'List Linear projects, optionally filtered by team',
    input_schema: {
      type: 'object',
      properties: { teamId: { type: 'string' } },
    },
  },
  {
    name: 'mcp__claude_ai_Linear__get_project',
    description: 'Get a Linear project by ID',
    input_schema: {
      type: 'object',
      properties: { id: { type: 'string' } },
      required: ['id'],
    },
  },
  {
    name: 'mcp__claude_ai_Linear__list_issue_statuses',
    description: 'List workflow states for a Linear team',
    input_schema: {
      type: 'object',
      properties: { teamId: { type: 'string' } },
      required: ['teamId'],
    },
  },
  {
    name: 'mcp__claude_ai_Linear__list_issue_labels',
    description: 'List issue labels for a Linear team',
    input_schema: {
      type: 'object',
      properties: { teamId: { type: 'string' } },
      required: ['teamId'],
    },
  },
  {
    name: 'mcp__claude_ai_Linear__list_documents',
    description: 'List Linear documents, optionally filtered by project',
    input_schema: {
      type: 'object',
      properties: { projectId: { type: 'string' } },
    },
  },
  {
    name: 'mcp__claude_ai_Linear__get_document',
    description: 'Get a Linear document by ID, including its content',
    input_schema: {
      type: 'object',
      properties: { id: { type: 'string' } },
      required: ['id'],
    },
  },
  {
    name: 'mcp__claude_ai_Linear__create_document',
    description: 'Create a new Linear document in a project',
    input_schema: {
      type: 'object',
      properties: {
        projectId: { type: 'string' },
        title:     { type: 'string' },
        content:   { type: 'string' },
      },
      required: ['projectId', 'title'],
    },
  },
];
