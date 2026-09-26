<script lang="ts">
    import ProjectDetail from "$lib/components/ProjectDetail.svelte";
    import { getProjectTool } from "$lib/data/projects";

    const tool = getProjectTool("swatch-search")!;
</script>

<ProjectDetail {tool} />
