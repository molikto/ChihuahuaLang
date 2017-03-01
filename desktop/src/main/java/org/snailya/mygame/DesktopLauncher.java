package org.snailya.mygame;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.backends.lwjgl3.Lwjgl3Application;
import com.badlogic.gdx.backends.lwjgl3.Lwjgl3ApplicationConfiguration;
import org.snailya.mygame.GameWrapper;

public class DesktopLauncher {
	public static void main (String[] arg) {
		Lwjgl3ApplicationConfiguration cfg = new Lwjgl3ApplicationConfiguration();
		cfg.setTitle("Chihuahua Language");
		cfg.setHdpiMode(Lwjgl3ApplicationConfiguration.HdpiMode.Pixels);
		// http://www.glfw.org/docs/latest/intro_guide.html
		// these is in screen coordinates
		// this is the smallest screen size we will support. the iPhone 1 size
//		int width = 480;
//		int height = 320;
        // iPhone 7 PLUS
		int width = 736;
		int height = 410;
		cfg.setWindowedMode(width, height);
		cfg.setIdleFPS(60);
		new Lwjgl3Application(new GameWrapper(width, 0), cfg);
		System.exit(0);
	}
}
