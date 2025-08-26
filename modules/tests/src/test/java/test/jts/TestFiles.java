/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package test.jts;

import java.io.File;

public class TestFiles {

    public static final String getResourceFilePath(String fileName) {
        ClassLoader classLoader = TestFiles.class.getClassLoader();
        return new File(classLoader.getResource("testdata/" + fileName).getFile()).getAbsolutePath();
    }
}
