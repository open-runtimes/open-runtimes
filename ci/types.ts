export interface BuildVersion {
    base: string;
    build_base?: string;
    args?: Record<string, string>;
    platforms?: string[];
    version_dir?: string;
}

export interface BuildConfig {
    image?: string;
    runtime_dir?: string;
    platforms?: string[];
    versions: Record<string, BuildVersion>;
}
