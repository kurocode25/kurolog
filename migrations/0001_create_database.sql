CREATE TABLE public.articles (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    user_id uuid NOT NULL,
    category_id uuid NOT NULL,
    slug character varying(64) NOT NULL,
    status integer DEFAULT 100 NOT NULL,
    title text DEFAULT ''::text NOT NULL,
    image character varying(512),
    markdown text DEFAULT ''::text NOT NULL,
    released_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now()
);

CREATE TABLE public.articles_tags (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    article_id uuid NOT NULL,
    tag_id uuid NOT NULL
);

CREATE TABLE public.categories (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    slug character varying(64) DEFAULT ''::character varying NOT NULL,
    name character varying(64) DEFAULT ''::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now(),
    last_posted_at timestamp with time zone DEFAULT now()
);

CREATE TABLE public.pages (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    slug character varying(64) NOT NULL,
    title character varying(255) DEFAULT ''::character varying NOT NULL,
    markdown text DEFAULT ''::text NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now(),
    pub_flag boolean DEFAULT false NOT NULL
);

CREATE TABLE public.tags (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    slug character varying(64) DEFAULT ''::character varying NOT NULL,
    name character varying(64) DEFAULT ''::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now()
);

CREATE TABLE public.users (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    code character varying(16) NOT NULL,
    name character varying(32) NOT NULL,
    email character varying(32) NOT NULL,
    password character varying(64) NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now()
);

ALTER TABLE ONLY public.articles
    ADD CONSTRAINT articles_pkey PRIMARY KEY (id);

ALTER TABLE ONLY public.articles
    ADD CONSTRAINT articles_slug UNIQUE (slug);

ALTER TABLE ONLY public.articles_tags
    ADD CONSTRAINT articles_tags_pkey PRIMARY KEY (id);

ALTER TABLE ONLY public.categories
    ADD CONSTRAINT categories_name_key UNIQUE (name);

ALTER TABLE ONLY public.categories
    ADD CONSTRAINT categories_pkey PRIMARY KEY (id);

ALTER TABLE ONLY public.categories
    ADD CONSTRAINT categories_slug_key UNIQUE (slug);

ALTER TABLE ONLY public.pages
    ADD CONSTRAINT pages_pkey PRIMARY KEY (id);

ALTER TABLE ONLY public.pages
    ADD CONSTRAINT pages_slug_key UNIQUE (slug);

ALTER TABLE ONLY public.tags
    ADD CONSTRAINT tags_name_key UNIQUE (name);

ALTER TABLE ONLY public.tags
    ADD CONSTRAINT tags_pkey PRIMARY KEY (id);

ALTER TABLE ONLY public.tags
    ADD CONSTRAINT tags_slug_key UNIQUE (slug);

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_code_key UNIQUE (code);

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);

ALTER TABLE ONLY public.articles_tags
    ADD CONSTRAINT article_id FOREIGN KEY (article_id) REFERENCES public.articles(id) ON DELETE CASCADE;

ALTER TABLE ONLY public.articles
    ADD CONSTRAINT category_id FOREIGN KEY (category_id) REFERENCES public.categories(id);

ALTER TABLE ONLY public.articles_tags
    ADD CONSTRAINT tag_id FOREIGN KEY (tag_id) REFERENCES public.tags(id) ON DELETE CASCADE;

ALTER TABLE ONLY public.articles
    ADD CONSTRAINT user_id FOREIGN KEY (user_id) REFERENCES public.users(id);

