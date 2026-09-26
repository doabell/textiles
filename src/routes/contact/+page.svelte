<script lang="ts">
    function composeEmail(event: SubmitEvent) {
        event.preventDefault();
        const form = event.currentTarget as HTMLFormElement;
        const values = new FormData(form);
        const name = String(values.get("name") ?? "");
        const email = String(values.get("email") ?? "");
        const message = String(values.get("message") ?? "");
        const subject = encodeURIComponent("Dutch Textile Trade Project");
        const body = encodeURIComponent(`${message}\n\nFrom: ${name}\nReply to: ${email}`);
        window.location.href = `mailto:carriea@middlebury.edu?subject=${subject}&body=${body}`;
    }
</script>

<svelte:head>
    <title>Contact — Dutch Textile Trade Project</title>
    <meta name="description" content="Contact" />
</svelte:head>

<div class="page-shell">
    <header class="page-intro">
        <div>
            <h1>Contact</h1>
        </div>
    </header>
</div>

<section class="contact-section page-shell">
    <form onsubmit={composeEmail}>
        <label>
            <span>Your name</span>
            <input name="name" type="text" autocomplete="name" required />
        </label>
        <label>
            <span>Your email</span>
            <input name="email" type="email" autocomplete="email" required />
        </label>
        <label>
            <span>Your message</span>
            <textarea name="message" rows="10" required></textarea>
        </label>
        <button class="button" type="submit">Compose email</button>
    </form>
</section>

<style>
    .contact-section {
        max-width: 65ch;
        padding-top: clamp(2rem, 4vw, 4rem);
        padding-bottom: clamp(6rem, 11vw, 11rem);
    }

    form {
        padding-top: 1rem;
        border-top: 1px solid var(--line-strong);
    }

    label {
        display: block;
        margin-bottom: 1.3rem;
    }

    label span {
        display: block;
        margin-bottom: 0.45rem;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.8125rem;
        letter-spacing: 0.035em;
        text-transform: uppercase;
    }

    input,
    textarea {
        width: 100%;
        padding: 0.8rem;
        color: var(--ink);
        border: 1px solid var(--line-strong);
        border-radius: 0;
        outline: 0;
        background: var(--cream);
    }

    input {
        min-height: 3.1rem;
    }

    textarea {
        resize: vertical;
    }

    input:focus,
    textarea:focus {
        border-color: var(--madder);
    }

    form .button {
        cursor: pointer;
    }
</style>
