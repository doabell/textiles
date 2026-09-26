import { sveltekit } from "@sveltejs/kit/vite";
import { defineConfig } from "vite";

export default defineConfig({
    plugins: [sveltekit()],
    server: {
        fs: {
            // Source images and downloads are imported from the legacy research folders.
            allow: ["data", "pictures/img", "pictures/datasets"],
        },
    },
});
